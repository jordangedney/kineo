// Caps Lock as hyper. Two parts:
//
// 1. The HID layer remaps Caps Lock to F18, so the Caps Lock toggle (and
//    its light) never happens. Same mechanism as `hidutil property --set`.
// 2. An event tap at the HID level swallows F18 and, while it is held,
//    adds cmd+alt+ctrl to every key event. The tap sits in front of the
//    window server's hotkey matching, so global shortcuts see a real chord.
//
// The tap runs on a thread of its own and never calls into Haskell: macOS
// disables taps that are slow to answer, and Kineo's main thread can block
// for up to a second on an unresponsive app.

#import <ApplicationServices/ApplicationServices.h>
#import <Foundation/Foundation.h>
#import <IOKit/hidsystem/IOHIDEventSystemClient.h>

#include "hyper.h"

// HID usages (page 7, keyboard) and the matching virtual key code.
static const uint64_t kUsageCapsLock = 0x700000039;
static const uint64_t kUsageF18 = 0x70000006D;
static const CGKeyCode kKeyF18 = 0x4F;
static const CGKeyCode kKeyEscape = 0x35;

static const CGEventFlags kHyperFlags = kCGEventFlagMaskCommand | kCGEventFlagMaskAlternate | kCGEventFlagMaskControl;
static const double kTapWindow = 0.25;  // seconds: longer than this is a hold, not a tap

bool kh_trusted(bool prompt) {
    NSDictionary *opts = @{(__bridge NSString *)kAXTrustedCheckOptionPrompt : @(prompt)};
    return AXIsProcessTrustedWithOptions((__bridge CFDictionaryRef)opts);
}

// Mapping ------------------------------------------------------------------

static IOHIDEventSystemClientRef g_client;
static NSArray *g_previous;  // the UserKeyMapping we replaced; nil while not installed

static bool is_ours(NSDictionary *entry) {
    return [entry[@"HIDKeyboardModifierMappingSrc"] unsignedLongLongValue] == kUsageCapsLock &&
           [entry[@"HIDKeyboardModifierMappingDst"] unsignedLongLongValue] == kUsageF18;
}

static bool install_mapping(void) {
    if (g_previous) return true;
    if (!g_client) g_client = IOHIDEventSystemClientCreateSimpleClient(kCFAllocatorDefault);
    if (!g_client) return false;
    id current = CFBridgingRelease(IOHIDEventSystemClientCopyProperty(g_client, CFSTR("UserKeyMapping")));

    // What to put back later. A Caps Lock -> F18 entry can only be left
    // over from a Kineo that was killed, so it is not part of that.
    NSMutableArray *previous = [NSMutableArray array];
    if ([current isKindOfClass:[NSArray class]])
        for (NSDictionary *entry in current)
            if (!is_ours(entry)) [previous addObject:entry];

    NSMutableArray *mapping = [NSMutableArray array];
    for (NSDictionary *entry in previous)
        if ([entry[@"HIDKeyboardModifierMappingSrc"] unsignedLongLongValue] != kUsageCapsLock) [mapping addObject:entry];
    [mapping addObject:@{
        @"HIDKeyboardModifierMappingSrc" : @(kUsageCapsLock),
        @"HIDKeyboardModifierMappingDst" : @(kUsageF18),
    }];
    if (!IOHIDEventSystemClientSetProperty(g_client, CFSTR("UserKeyMapping"), (__bridge CFArrayRef)mapping))
        return false;
    g_previous = previous;
    return true;
}

static void restore_mapping(void) {
    if (!g_client || !g_previous) return;
    IOHIDEventSystemClientSetProperty(g_client, CFSTR("UserKeyMapping"), (__bridge CFArrayRef)g_previous);
    g_previous = nil;
}

// Event tap ----------------------------------------------------------------

static CFMachPortRef g_tap;
static _Atomic bool g_on;
static _Atomic bool g_escape;
static bool g_held;
static bool g_used;  // was another key pressed during this hold?
static CFAbsoluteTime g_pressed_at;

static void post_key(CGKeyCode key) {
    CGEventRef down = CGEventCreateKeyboardEvent(NULL, key, true);
    CGEventRef up = CGEventCreateKeyboardEvent(NULL, key, false);
    CGEventPost(kCGHIDEventTap, down);
    CGEventPost(kCGHIDEventTap, up);
    CFRelease(down);
    CFRelease(up);
}

static CGEventRef tap_callback(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *data) {
    (void)proxy;
    (void)data;
    if (type == kCGEventTapDisabledByTimeout || type == kCGEventTapDisabledByUserInput) {
        if (g_on) CGEventTapEnable(g_tap, true);
        return event;
    }
    if (type != kCGEventKeyDown && type != kCGEventKeyUp) return event;

    CGKeyCode key = (CGKeyCode)CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
    if (key == kKeyF18) {
        if (type == kCGEventKeyDown && !g_held) {  // ignore auto-repeat
            g_held = true;
            g_used = false;
            g_pressed_at = CFAbsoluteTimeGetCurrent();
        } else if (type == kCGEventKeyUp) {
            g_held = false;
            if (g_escape && !g_used && CFAbsoluteTimeGetCurrent() - g_pressed_at < kTapWindow) post_key(kKeyEscape);
        }
        return NULL;
    }
    if (g_held) {
        if (type == kCGEventKeyDown) g_used = true;
        CGEventSetFlags(event, CGEventGetFlags(event) | kHyperFlags);
    }
    return event;
}

static bool start_tap(void) {
    if (g_tap) {
        CGEventTapEnable(g_tap, true);
        return true;
    }
    CGEventMask mask = CGEventMaskBit(kCGEventKeyDown) | CGEventMaskBit(kCGEventKeyUp);
    g_tap = CGEventTapCreate(kCGHIDEventTap, kCGHeadInsertEventTap, kCGEventTapOptionDefault, mask, tap_callback, NULL);
    if (!g_tap) return false;
    CFRunLoopSourceRef source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, g_tap, 0);
    NSThread *thread = [[NSThread alloc] initWithBlock:^{
        CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopCommonModes);
        CFRelease(source);
        CFRunLoopRun();
    }];
    thread.name = @"kineo hyper key";
    thread.qualityOfService = NSQualityOfServiceUserInteractive;
    [thread start];
    return true;
}

bool kh_configure(bool on, bool escape) {
    static dispatch_once_t once;
    static NSObject *lock;
    dispatch_once(&once, ^{
        lock = [NSObject new];
        atexit(restore_mapping);
    });
    @synchronized(lock) {
        g_escape = escape;
        g_on = on;
        if (!on) {
            if (g_tap) CGEventTapEnable(g_tap, false);
            g_held = false;
            restore_mapping();
            return true;
        }
        // The tap first: with the mapping but no tap, Caps Lock would do nothing.
        if (!start_tap()) {
            g_on = false;
            return false;
        }
        if (!install_mapping()) {
            g_on = false;
            CGEventTapEnable(g_tap, false);
            return false;
        }
        return true;
    }
}
