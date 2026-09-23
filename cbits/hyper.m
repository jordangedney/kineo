// Caps Lock as hyper. Two parts:
//
// 1. The HID layer remaps Caps Lock to F18, so the Caps Lock toggle (and
//    its light) never happens. Same mechanism as `hidutil property --set`.
// 2. An event tap at the HID level swallows F18 and, while it is held,
//    adds cmd+alt+ctrl to every key event. The tap sits in front of the
//    window server's hotkey matching, so global shortcuts see a real chord.
//
// The tap callback runs on the main run loop and never calls into Haskell:
// macOS disables taps that are slow to answer.

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
static id g_previous;  // the UserKeyMapping we replaced (nil if none)

bool kh_install_mapping(void) {
    if (!g_client) g_client = IOHIDEventSystemClientCreateSimpleClient(kCFAllocatorDefault);
    if (!g_client) return false;
    g_previous = CFBridgingRelease(IOHIDEventSystemClientCopyProperty(g_client, CFSTR("UserKeyMapping")));

    NSMutableArray *mapping = [NSMutableArray array];
    if ([g_previous isKindOfClass:[NSArray class]])
        for (NSDictionary *entry in g_previous)
            if ([entry[@"HIDKeyboardModifierMappingSrc"] unsignedLongLongValue] != kUsageCapsLock)
                [mapping addObject:entry];
    [mapping addObject:@{
        @"HIDKeyboardModifierMappingSrc" : @(kUsageCapsLock),
        @"HIDKeyboardModifierMappingDst" : @(kUsageF18),
    }];
    return IOHIDEventSystemClientSetProperty(g_client, CFSTR("UserKeyMapping"), (__bridge CFArrayRef)mapping);
}

void kh_restore_mapping(void) {
    if (!g_client) return;
    id previous = [g_previous isKindOfClass:[NSArray class]] ? g_previous : @[];
    IOHIDEventSystemClientSetProperty(g_client, CFSTR("UserKeyMapping"), (__bridge CFTypeRef)previous);
}

// Event tap ----------------------------------------------------------------

static CFMachPortRef g_tap;
static bool g_escape;
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
        CGEventTapEnable(g_tap, true);
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

bool kh_start_tap(bool escape) {
    g_escape = escape;
    CGEventMask mask = CGEventMaskBit(kCGEventKeyDown) | CGEventMaskBit(kCGEventKeyUp);
    g_tap = CGEventTapCreate(kCGHIDEventTap, kCGHeadInsertEventTap, kCGEventTapOptionDefault, mask, tap_callback, NULL);
    if (!g_tap) return false;
    CFRunLoopSourceRef source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, g_tap, 0);
    CFRunLoopAddSource(CFRunLoopGetMain(), source, kCFRunLoopCommonModes);
    CFRelease(source);
    CGEventTapEnable(g_tap, true);
    return true;
}

void kh_run(void) {
    CFRunLoopRun();
}
