// macOS platform layer for Kineo. See kineo.h for the contract.
//
// Threading: observers, notifications and hotkeys all fire on the main
// thread's run loop. The window table is shared with Haskell worker threads
// (which move windows), so it is only touched under @synchronized.

#import <AppKit/AppKit.h>
#import <ApplicationServices/ApplicationServices.h>
#import <Carbon/Carbon.h>
#import <CoreVideo/CoreVideo.h>
#import <ServiceManagement/ServiceManagement.h>
#import <dlfcn.h>
#import <mach/mach_time.h>
#import <pthread.h>
#import <sys/time.h>

#include "kineo.h"

// Private symbols, resolved at runtime so a missing one degrades a feature
// instead of breaking the build or the launch. ----------------------------

static int (*SLSMainConnectionID)(void);
static uint64_t (*SLSManagedDisplayGetCurrentSpace)(int cid, CFStringRef display);
static CFArrayRef (*SLSCopySpacesForWindows)(int cid, int mask, CFArrayRef wids);
static int (*SLSSpaceGetType)(int cid, uint64_t sid);
static AXError (*AXUIElementGetWindow)(AXUIElementRef el, uint32_t *wid);
static CFUUIDRef (*CGDisplayCreateUUID)(uint32_t display);

static void load_private_symbols(void) {
    static dispatch_once_t once;
    dispatch_once(&once, ^{
        void *sl = dlopen("/System/Library/PrivateFrameworks/SkyLight.framework/SkyLight", RTLD_LAZY);
        if (sl) {
            SLSMainConnectionID = (int (*)(void))dlsym(sl, "SLSMainConnectionID");
            SLSManagedDisplayGetCurrentSpace =
                (uint64_t (*)(int, CFStringRef))dlsym(sl, "SLSManagedDisplayGetCurrentSpace");
            SLSCopySpacesForWindows = (CFArrayRef (*)(int, int, CFArrayRef))dlsym(sl, "SLSCopySpacesForWindows");
            SLSSpaceGetType = (int (*)(int, uint64_t))dlsym(sl, "SLSSpaceGetType");
        }
        AXUIElementGetWindow = (AXError (*)(AXUIElementRef, uint32_t *))dlsym(RTLD_DEFAULT, "_AXUIElementGetWindow");
        CGDisplayCreateUUID = (CFUUIDRef (*)(uint32_t))dlsym(RTLD_DEFAULT, "CGDisplayCreateUUIDFromDisplayID");
        if (!SLSMainConnectionID || !SLSManagedDisplayGetCurrentSpace || !SLSCopySpacesForWindows)
            fprintf(stderr, "kineo: SkyLight symbols missing; Spaces support is degraded\n");
        if (!AXUIElementGetWindow)
            fprintf(stderr, "kineo: _AXUIElementGetWindow missing; windows cannot be identified\n");
    });
}

static int connection(void) {
    static int cid = 0;
    if (!cid && SLSMainConnectionID) cid = SLSMainConnectionID();
    return cid;
}

// State ------------------------------------------------------------------

@interface KNWindow : NSObject
@property(strong) id element;  // AXUIElementRef
@property pid_t pid;
@property uint32_t wid;
@end
@implementation KNWindow
@end

@interface KNApp : NSObject
@property(strong) id observer;  // AXObserverRef
@property(strong) id element;   // AXUIElementRef of the application
@property pid_t pid;
@property uint32_t lastFocus;  // the window last reported focused
@end
@implementation KNApp
@end

static kn_event_fn g_emit;
static NSMutableDictionary<NSNumber *, KNWindow *> *g_windows;  // under @synchronized(g_windows)
static NSMutableDictionary<NSNumber *, KNApp *> *g_apps;        // main thread only

static void emit(int32_t kind, pid_t pid, uint32_t arg) {
    if (g_emit) g_emit(kind, pid, arg);
}

static KNWindow *window_for(uint32_t wid) {
    @synchronized(g_windows) {
        return g_windows[@(wid)];
    }
}

// Accessibility helpers ---------------------------------------------------

static const float kMessagingTimeout = 1.0;  // seconds; stops a hung app hanging us

static uint32_t wid_of(AXUIElementRef el) {
    uint32_t wid = 0;
    if (AXUIElementGetWindow && AXUIElementGetWindow(el, &wid) == kAXErrorSuccess) return wid;
    return 0;
}

static id copy_attr(AXUIElementRef el, CFStringRef attr) {
    CFTypeRef value = NULL;
    if (AXUIElementCopyAttributeValue(el, attr, &value) != kAXErrorSuccess) return nil;
    return CFBridgingRelease(value);
}

static bool bool_attr(AXUIElementRef el, CFStringRef attr) {
    id v = copy_attr(el, attr);
    return [v isKindOfClass:[NSNumber class]] && [v boolValue];
}

static bool settable(AXUIElementRef el, CFStringRef attr) {
    Boolean ok = false;
    return AXUIElementIsAttributeSettable(el, attr, &ok) == kAXErrorSuccess && ok;
}

static bool element_frame(AXUIElementRef el, kn_rect *out) {
    id pos = copy_attr(el, kAXPositionAttribute);
    id size = copy_attr(el, kAXSizeAttribute);
    CGPoint p;
    CGSize s;
    if (!pos || !size || !AXValueGetValue((__bridge AXValueRef)pos, kAXValueCGPointType, &p) ||
        !AXValueGetValue((__bridge AXValueRef)size, kAXValueCGSizeType, &s))
        return false;
    *out = (kn_rect){p.x, p.y, s.width, s.height};
    return true;
}

static void copy_utf8(NSString *s, char *buf, size_t len) {
    buf[0] = 0;
    if ([s isKindOfClass:[NSString class]]) strlcpy(buf, s.UTF8String ?: "", len);
}

// Window tracking ---------------------------------------------------------

static CFStringRef const kWindowNotifications[] = {
    kAXUIElementDestroyedNotification, kAXWindowMovedNotification,         kAXWindowResizedNotification,
    kAXWindowMiniaturizedNotification, kAXWindowDeminiaturizedNotification,
};

// Start tracking a window element. Returns its id, or 0 if it is not a
// window we can identify. With an app observer, subscribes to the window's
// own notifications (close, move, ...), which only fire per element.
static uint32_t track_window(pid_t pid, AXUIElementRef el, bool announce) {
    NSString *role = copy_attr(el, kAXRoleAttribute);
    if (![role isEqual:(__bridge NSString *)kAXWindowRole]) return 0;
    uint32_t wid = wid_of(el);
    if (!wid) return 0;

    @synchronized(g_windows) {
        if (g_windows[@(wid)]) return wid;
        KNWindow *w = [KNWindow new];
        w.element = (__bridge id)el;
        w.pid = pid;
        w.wid = wid;
        g_windows[@(wid)] = w;
    }
    AXUIElementSetMessagingTimeout(el, kMessagingTimeout);

    KNApp *app = g_apps[@(pid)];
    if (app) {
        AXObserverRef obs = (__bridge AXObserverRef)app.observer;
        for (size_t i = 0; i < sizeof kWindowNotifications / sizeof *kWindowNotifications; i++)
            AXObserverAddNotification(obs, el, kWindowNotifications[i], (void *)(intptr_t)pid);
    }
    if (announce) emit(KN_WINDOW_CREATED, pid, wid);
    return wid;
}

static uint32_t untrack_element(pid_t pid, AXUIElementRef el) {
    @synchronized(g_windows) {
        for (NSNumber *key in g_windows.allKeys) {
            KNWindow *w = g_windows[key];
            if (w.pid == pid && CFEqual((__bridge CFTypeRef)w.element, el)) {
                [g_windows removeObjectForKey:key];
                return w.wid;
            }
        }
    }
    return 0;
}

static uint32_t known_wid(pid_t pid, AXUIElementRef el) {
    uint32_t wid = wid_of(el);
    if (wid && window_for(wid)) return wid;
    @synchronized(g_windows) {
        for (KNWindow *w in g_windows.allValues)
            if (w.pid == pid && CFEqual((__bridge CFTypeRef)w.element, el)) return w.wid;
    }
    return 0;
}

static void scan_app_windows(pid_t pid, AXUIElementRef app, bool announce) {
    NSArray *windows = copy_attr(app, kAXWindowsAttribute);
    if (![windows isKindOfClass:[NSArray class]]) return;
    for (id el in windows) track_window(pid, (__bridge AXUIElementRef)el, announce);
}

static bool is_frontmost(pid_t pid) {
    return NSWorkspace.sharedWorkspace.frontmostApplication.processIdentifier == pid;
}

static pid_t g_focus_pid;  // the app of the last focus reported; main thread only

static void announce_focus(pid_t pid) {
    // Every activation counts, Kineo's own included, so coming back to the
    // same window after one is still reported.
    bool again = pid == g_focus_pid;
    g_focus_pid = pid;
    AXUIElementRef app = AXUIElementCreateApplication(pid);
    if (!app) return;
    id focused = copy_attr(app, kAXFocusedWindowAttribute);
    CFRelease(app);
    if (!focused) return;
    uint32_t wid = track_window(pid, (__bridge AXUIElementRef)focused, true);
    // Activation can be announced twice; the window already has focus.
    if (!wid || (again && wid == g_apps[@(pid)].lastFocus)) return;
    g_apps[@(pid)].lastFocus = wid;
    emit(KN_WINDOW_FOCUSED, pid, wid);
}

// Focus moved within an app. Activating it, focusing a window and the
// element inside it all say so; report each window once.
static void focus_moved(pid_t pid, uint32_t wid) {
    KNApp *a = g_apps[@(pid)];
    if (!wid || !a || wid == a.lastFocus || !is_frontmost(pid)) return;
    a.lastFocus = wid;
    g_focus_pid = pid;
    emit(KN_WINDOW_FOCUSED, pid, wid);
}

static void ax_callback(AXObserverRef observer, AXUIElementRef el, CFStringRef note, void *refcon) {
    (void)observer;
    pid_t pid = (pid_t)(intptr_t)refcon;
    if (CFEqual(note, kAXWindowCreatedNotification)) {
        track_window(pid, el, true);
    } else if (CFEqual(note, kAXFocusedWindowChangedNotification)) {
        // Apps move focus between their own windows in the background too;
        // only the frontmost app's focus is the user's.
        focus_moved(pid, track_window(pid, el, true));
    } else if (CFEqual(note, kAXFocusedUIElementChangedNotification)) {
        // Switching native tabs changes the focused window without saying
        // so; only the focused element inside it is announced.
        id win = copy_attr(el, kAXWindowAttribute);
        if (win) focus_moved(pid, track_window(pid, (__bridge AXUIElementRef)win, true));
    } else if (CFEqual(note, kAXUIElementDestroyedNotification)) {
        uint32_t wid = untrack_element(pid, el);
        if (wid) emit(KN_WINDOW_DESTROYED, pid, wid);
    } else {
        uint32_t wid = known_wid(pid, el);
        if (!wid) return;
        if (CFEqual(note, kAXWindowMovedNotification)) emit(KN_WINDOW_MOVED, pid, wid);
        else if (CFEqual(note, kAXWindowResizedNotification)) emit(KN_WINDOW_RESIZED, pid, wid);
        else if (CFEqual(note, kAXWindowMiniaturizedNotification)) emit(KN_WINDOW_MINIMIZED, pid, wid);
        else if (CFEqual(note, kAXWindowDeminiaturizedNotification)) emit(KN_WINDOW_DEMINIMIZED, pid, wid);
    }
}

// Applications ------------------------------------------------------------

static void observe_app(NSRunningApplication *running, int attempt);

static void retry_observe(NSRunningApplication *running, int attempt) {
    // Freshly launched apps often refuse accessibility for a moment.
    if (attempt >= 20 || running.terminated) return;
    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 250 * NSEC_PER_MSEC), dispatch_get_main_queue(), ^{
        observe_app(running, attempt + 1);
    });
}

static void observe_app(NSRunningApplication *running, int attempt) {
    pid_t pid = running.processIdentifier;
    if (running.activationPolicy != NSApplicationActivationPolicyRegular || pid == getpid() || g_apps[@(pid)])
        return;

    AXObserverRef observer = NULL;
    if (AXObserverCreate(pid, ax_callback, &observer) != kAXErrorSuccess) return retry_observe(running, attempt);
    AXUIElementRef app = AXUIElementCreateApplication(pid);
    AXUIElementSetMessagingTimeout(app, kMessagingTimeout);

    CFStringRef notes[] = {kAXWindowCreatedNotification, kAXFocusedWindowChangedNotification};
    for (size_t i = 0; i < 2; i++) {
        AXError err = AXObserverAddNotification(observer, app, notes[i], (void *)(intptr_t)pid);
        if (err != kAXErrorSuccess && err != kAXErrorNotificationAlreadyRegistered) {
            CFRelease(observer);
            CFRelease(app);
            if (err != kAXErrorNotificationUnsupported) retry_observe(running, attempt);
            return;
        }
    }
    // Optional: only for noticing tab switches.
    AXObserverAddNotification(observer, app, kAXFocusedUIElementChangedNotification, (void *)(intptr_t)pid);
    CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), kCFRunLoopDefaultMode);

    KNApp *a = [KNApp new];
    a.observer = CFBridgingRelease(observer);
    a.element = CFBridgingRelease(app);
    a.pid = pid;
    g_apps[@(pid)] = a;
    scan_app_windows(pid, (__bridge AXUIElementRef)a.element, true);
}

static void forget_app(pid_t pid) {
    KNApp *a = g_apps[@(pid)];
    if (a) {
        CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource((__bridge AXObserverRef)a.observer),
                              kCFRunLoopDefaultMode);
        [g_apps removeObjectForKey:@(pid)];
    }
    @synchronized(g_windows) {
        for (NSNumber *key in g_windows.allKeys)
            if (g_windows[key].pid == pid) [g_windows removeObjectForKey:key];
    }
}

// Hotkeys -----------------------------------------------------------------

static NSMutableArray<NSValue *> *g_hotkeys;

static OSStatus hotkey_handler(EventHandlerCallRef next, EventRef event, void *data) {
    (void)next;
    (void)data;
    EventHotKeyID hk;
    if (GetEventParameter(event, kEventParamDirectObject, typeEventHotKeyID, NULL, sizeof hk, NULL, &hk) == noErr)
        emit(KN_HOTKEY, 0, hk.id);
    return noErr;
}

void kn_set_hotkeys(const kn_hotkey *keys, int n) {
    NSData *copy = [NSData dataWithBytes:keys length:sizeof(kn_hotkey) * (size_t)n];
    dispatch_async(dispatch_get_main_queue(), ^{
        static bool installed = false;
        if (!installed) {
            EventTypeSpec spec = {kEventClassKeyboard, kEventHotKeyPressed};
            InstallEventHandler(GetApplicationEventTarget(), hotkey_handler, 1, &spec, NULL, NULL);
            g_hotkeys = [NSMutableArray new];
            installed = true;
        }
        for (NSValue *v in g_hotkeys) UnregisterEventHotKey(v.pointerValue);
        [g_hotkeys removeAllObjects];

        const kn_hotkey *ks = copy.bytes;
        for (int i = 0; i < n; i++) {
            EventHotKeyRef ref = NULL;
            EventHotKeyID hk = {.signature = 0x4B4E454F /* KNEO */, .id = (UInt32)i};
            OSStatus err = RegisterEventHotKey(ks[i].keycode, ks[i].modifiers, hk, GetApplicationEventTarget(), 0, &ref);
            if (err == noErr) [g_hotkeys addObject:[NSValue valueWithPointer:ref]];
            else
                fprintf(stderr, "kineo: could not register hotkey (key code %u, modifiers 0x%x): error %d\n",
                        ks[i].keycode, ks[i].modifiers, (int)err);
        }
    });
}

// Displays and spaces -------------------------------------------------------

static uint64_t current_space(CGDirectDisplayID did) {
    if (!SLSManagedDisplayGetCurrentSpace || !CGDisplayCreateUUID || !connection()) return 0;
    uint64_t sid = 0;
    CFUUIDRef uuid = CGDisplayCreateUUID(did);
    if (uuid) {
        CFStringRef str = CFUUIDCreateString(NULL, uuid);
        sid = SLSManagedDisplayGetCurrentSpace(connection(), str);
        CFRelease(str);
        CFRelease(uuid);
    }
    // With "Displays have separate Spaces" off every display is "Main".
    if (!sid) sid = SLSManagedDisplayGetCurrentSpace(connection(), CFSTR("Main"));
    return sid;
}

static kn_rect flip(NSRect r, CGFloat primary_height) {
    return (kn_rect){r.origin.x, primary_height - r.origin.y - r.size.height, r.size.width, r.size.height};
}

static void on_main(void (^block)(void)) {
    if (NSThread.isMainThread) block();
    else dispatch_sync(dispatch_get_main_queue(), block);
}

int kn_displays(kn_display *out, int max) {
    __block int n = 0;
    on_main(^{
        NSArray<NSScreen *> *screens = NSScreen.screens;
        if (screens.count == 0) return;
        // Cocoa's origin is the bottom-left of the primary (first) screen.
        CGFloat primary = screens[0].frame.size.height;
        for (NSScreen *s in screens) {
            if (n >= max) break;
            CGDirectDisplayID did = [s.deviceDescription[@"NSScreenNumber"] unsignedIntValue];
            uint64_t sid = current_space(did);
            out[n] = (kn_display){
                .id = did,
                .frame = flip(s.frame, primary),
                .visible = flip(s.visibleFrame, primary),
                .space = sid,
                .user_space = !(SLSSpaceGetType && sid && SLSSpaceGetType(connection(), sid) != 0),
            };
            n++;
        }
    });
    return n;
}

void kn_window_spaces(const uint32_t *wids, uint64_t *out, int n) {
    for (int i = 0; i < n; i++) {
        out[i] = 0;
        if (!SLSCopySpacesForWindows || !connection()) continue;
        CFArrayRef spaces = SLSCopySpacesForWindows(connection(), 0x7, (__bridge CFArrayRef) @[ @(wids[i]) ]);
        if (!spaces) continue;
        if (CFArrayGetCount(spaces) > 0)
            out[i] = [(__bridge NSNumber *)CFArrayGetValueAtIndex(spaces, 0) unsignedLongLongValue];
        CFRelease(spaces);
    }
}

// Windows -------------------------------------------------------------------

bool kn_query_window(uint32_t wid, kn_window_info *out) {
    KNWindow *w = window_for(wid);
    if (!w) return false;
    AXUIElementRef el = (__bridge AXUIElementRef)w.element;
    memset(out, 0, sizeof *out);
    out->pid = w.pid;
    if (!element_frame(el, &out->frame)) return false;
    NSString *role = copy_attr(el, kAXRoleAttribute);
    NSString *subrole = copy_attr(el, kAXSubroleAttribute);
    out->standard = [role isEqual:(__bridge NSString *)kAXWindowRole] &&
                    [subrole isEqual:(__bridge NSString *)kAXStandardWindowSubrole];
    out->resizable = settable(el, kAXSizeAttribute);
    out->movable = settable(el, kAXPositionAttribute);
    out->minimized = bool_attr(el, kAXMinimizedAttribute);
    out->fullscreen = bool_attr(el, CFSTR("AXFullScreen"));
    copy_utf8(copy_attr(el, kAXTitleAttribute), out->title, sizeof out->title);
    copy_utf8([NSRunningApplication runningApplicationWithProcessIdentifier:w.pid].bundleIdentifier, out->bundle_id,
              sizeof out->bundle_id);
    kn_window_spaces(&wid, &out->space, 1);
    return true;
}

bool kn_window_frame(uint32_t wid, kn_rect *out) {
    KNWindow *w = window_for(wid);
    return w && element_frame((__bridge AXUIElementRef)w.element, out);
}

int kn_window_set_frame(uint32_t wid, const kn_rect *r, int32_t what) {
    KNWindow *w = window_for(wid);
    if (!w) return KN_UNKNOWN_WINDOW;
    AXUIElementRef el = (__bridge AXUIElementRef)w.element;
    AXError err = kAXErrorSuccess;
    if (what & KN_SET_POSITION) {
        CGPoint p = {r->x, r->y};
        AXValueRef v = AXValueCreate(kAXValueCGPointType, &p);
        err = AXUIElementSetAttributeValue(el, kAXPositionAttribute, v);
        CFRelease(v);
    }
    if (err == kAXErrorSuccess && (what & KN_SET_SIZE)) {
        CGSize s = {r->w, r->h};
        AXValueRef v = AXValueCreate(kAXValueCGSizeType, &s);
        err = AXUIElementSetAttributeValue(el, kAXSizeAttribute, v);
        CFRelease(v);
    }
    if (err == kAXErrorSuccess) return KN_OK;
    return err == kAXErrorInvalidUIElement ? KN_DEAD_WINDOW : KN_FAILED;
}

void kn_window_focus(uint32_t wid) {
    KNWindow *w = window_for(wid);
    if (!w) return;
    AXUIElementRef el = (__bridge AXUIElementRef)w.element;
    AXUIElementSetAttributeValue(el, kAXMainAttribute, kCFBooleanTrue);
    AXUIElementPerformAction(el, kAXRaiseAction);
    AXUIElementRef app = AXUIElementCreateApplication(w.pid);
    AXUIElementSetAttributeValue(app, kAXFrontmostAttribute, kCFBooleanTrue);
    CFRelease(app);
    pid_t pid = w.pid;
    dispatch_async(dispatch_get_main_queue(), ^{
        [[NSRunningApplication runningApplicationWithProcessIdentifier:pid] activateWithOptions:0];
    });
}

void kn_window_close(uint32_t wid) {
    KNWindow *w = window_for(wid);
    if (!w) return;
    AXUIElementRef el = (__bridge AXUIElementRef)w.element;
    CFTypeRef button = NULL;
    if (AXUIElementCopyAttributeValue(el, kAXCloseButtonAttribute, &button) == kAXErrorSuccess && button) {
        AXUIElementPerformAction((AXUIElementRef)button, kAXPressAction);
        CFRelease(button);
    }
}

void kn_focus_nothing(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        [NSApp activateIgnoringOtherApps:YES];
    });
}

int kn_scan_windows(uint32_t *out, int max) {
    __block int n = 0;
    on_main(^{
        for (NSRunningApplication *running in NSWorkspace.sharedWorkspace.runningApplications) {
            if (running.activationPolicy != NSApplicationActivationPolicyRegular) continue;
            AXUIElementRef app = AXUIElementCreateApplication(running.processIdentifier);
            AXUIElementSetMessagingTimeout(app, kMessagingTimeout);
            NSArray *windows = copy_attr(app, kAXWindowsAttribute);
            CFRelease(app);
            if (![windows isKindOfClass:[NSArray class]]) continue;
            for (id el in windows) {
                uint32_t wid = track_window(running.processIdentifier, (__bridge AXUIElementRef)el, false);
                if (wid && n < max) out[n++] = wid;
            }
        }
    });
    return n;
}

// Frame pacing ----------------------------------------------------------------

// CVDisplayLink is deprecated in favour of NSScreen.displayLink, but that one
// fires on a run loop, and the main one is busy with notifications.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"

static pthread_mutex_t g_frame_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_frame_cond = PTHREAD_COND_INITIALIZER;
static CVDisplayLinkRef g_link;  // under g_frame_lock
static uint64_t g_frame_count;    // under g_frame_lock
static uint64_t g_frame_output;   // host time the latest frame is shown at, under g_frame_lock

static CVReturn on_frame(CVDisplayLinkRef link, const CVTimeStamp *now, const CVTimeStamp *output,
                         CVOptionFlags flags, CVOptionFlags *flagsOut, void *ctx) {
    (void)link, (void)now, (void)flags, (void)flagsOut, (void)ctx;
    pthread_mutex_lock(&g_frame_lock);
    g_frame_count++;
    g_frame_output = output->hostTime;
    pthread_cond_broadcast(&g_frame_cond);
    pthread_mutex_unlock(&g_frame_lock);
    return kCVReturnSuccess;
}

static double host_seconds(int64_t ticks) {
    static mach_timebase_info_data_t tb;
    if (!tb.denom) mach_timebase_info(&tb);
    return (double)ticks * tb.numer / tb.denom / 1e9;
}

double kn_next_frame(void) {
    pthread_mutex_lock(&g_frame_lock);
    if (!g_link) {
        if (CVDisplayLinkCreateWithCGDisplay(CGMainDisplayID(), &g_link) != kCVReturnSuccess) g_link = NULL;
        if (g_link) CVDisplayLinkSetOutputCallback(g_link, on_frame, NULL);
    }
    if (!g_link) {
        pthread_mutex_unlock(&g_frame_lock);
        return -1;
    }
    if (!CVDisplayLinkIsRunning(g_link)) CVDisplayLinkStart(g_link);
    // Wait for a fresh frame, but not forever: a sleeping display stops them.
    uint64_t seen = g_frame_count;
    struct timeval tv;
    gettimeofday(&tv, NULL);
    struct timespec deadline = {tv.tv_sec, tv.tv_usec * 1000 + 50 * 1000000};
    if (deadline.tv_nsec >= 1000000000) deadline.tv_sec++, deadline.tv_nsec -= 1000000000;
    while (g_frame_count == seen)
        if (pthread_cond_timedwait(&g_frame_cond, &g_frame_lock, &deadline)) break;
    double ahead = g_frame_count == seen ? -1 : fmax(0, host_seconds((int64_t)(g_frame_output - mach_absolute_time())));
    pthread_mutex_unlock(&g_frame_lock);
    return ahead;
}

void kn_frames_idle(void) {
    pthread_mutex_lock(&g_frame_lock);
    CVDisplayLinkRef link = g_link;
    g_link = NULL;
    pthread_mutex_unlock(&g_frame_lock);
    // Outside the lock: stopping waits for a callback in progress, which takes it.
    if (link) {
        CVDisplayLinkStop(link);
        CVDisplayLinkRelease(link);
    }
}

#pragma clang diagnostic pop

// Menu bar icon -----------------------------------------------------------------

// Menu actions go to Haskell as KN_MENU events, except "Open at Login",
// which is only about the app bundle and is handled here.
@interface KNMenu : NSObject <NSMenuDelegate, NSApplicationDelegate>
@property(strong) NSStatusItem *item;
@property(strong) NSMenuItem *login;
@property(strong) NSMenuItem *access;
@property(strong) NSMenu *menu;
@end

@implementation KNMenu

// A click pauses Kineo or carries on; a right-click (or ctrl-click) opens
// the menu. So does any click while Kineo is still waiting for access.
- (void)clicked:(id)sender {
    (void)sender;
    NSEvent *e = NSApp.currentEvent;
    if (!g_emit || e.type == NSEventTypeRightMouseUp || (e.modifierFlags & NSEventModifierFlagControl)) {
        self.item.menu = self.menu;
        [self.item.button performClick:nil];
        self.item.menu = nil;
    } else {
        emit(KN_MENU, 0, KN_MENU_TOGGLE_PAUSE);
    }
}

- (void)reload:(id)sender {
    (void)sender;
    emit(KN_MENU, 0, KN_MENU_RELOAD_CONFIG);
}

- (void)quit:(id)sender {
    (void)sender;
    if (!g_emit) exit(0);  // still waiting for Accessibility access: nothing to put back
    emit(KN_MENU, 0, KN_MENU_QUIT);
}

- (void)openAccessibility:(id)sender {
    (void)sender;
    [NSWorkspace.sharedWorkspace
        openURL:[NSURL URLWithString:@"x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility"]];
}

- (void)toggleLogin:(id)sender {
    (void)sender;
    SMAppService *app = SMAppService.mainAppService;
    NSError *error = nil;
    if (app.status == SMAppServiceStatusEnabled) [app unregisterAndReturnError:&error];
    else [app registerAndReturnError:&error];
    if (error) NSLog(@"kineo: open at login: %@", error.localizedDescription);
    if (app.status == SMAppServiceStatusRequiresApproval) [SMAppService openSystemSettingsLoginItems];
}

// Logging out, or `quit app "Kineo"`: quit as the Quit item would, which
// brings parked windows back first. kn_quit ends the process.
- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication *)sender {
    (void)sender;
    if (!g_emit) return NSTerminateNow;
    emit(KN_MENU, 0, KN_MENU_QUIT);
    return NSTerminateLater;
}

- (void)menuNeedsUpdate:(NSMenu *)menu {
    (void)menu;
    self.access.hidden = AXIsProcessTrusted();
    self.login.state = SMAppService.mainAppService.status == SMAppServiceStatusEnabled ? NSControlStateValueOn
                                                                                       : NSControlStateValueOff;
}

@end

static KNMenu *g_menu;

// Template images, so macOS draws them in the menu bar's own colour.
static NSString *const kIconRunning = @"rectangle.split.3x1.fill";
static NSString *const kIconPaused = @"rectangle.split.3x1";

static NSImage *status_icon(NSString *symbol) {
    NSImage *icon = [NSImage imageWithSystemSymbolName:symbol accessibilityDescription:@"Kineo"];
    icon.template = YES;
    return icon;
}

static void add_status_item(void) {
    if (g_menu) return;
    g_menu = [KNMenu new];
    NSMenu *menu = [NSMenu new];
    menu.delegate = g_menu;
    g_menu.access = [menu addItemWithTitle:@"Allow Accessibility Access…"
                                    action:@selector(openAccessibility:)
                             keyEquivalent:@""];
    g_menu.access.target = g_menu;
    NSMenuItem *item = [menu addItemWithTitle:@"Reload Config" action:@selector(reload:) keyEquivalent:@""];
    item.target = g_menu;
    // Starting at login only makes sense for Kineo.app, not a bare binary.
    if (kn_in_app_bundle()) {
        g_menu.login = [menu addItemWithTitle:@"Open at Login" action:@selector(toggleLogin:) keyEquivalent:@""];
        g_menu.login.target = g_menu;
    }
    [menu addItem:NSMenuItem.separatorItem];
    item = [menu addItemWithTitle:@"Quit Kineo" action:@selector(quit:) keyEquivalent:@""];
    item.target = g_menu;

    g_menu.menu = menu;
    g_menu.item = [NSStatusBar.systemStatusBar statusItemWithLength:NSSquareStatusItemLength];
    NSStatusBarButton *button = g_menu.item.button;
    button.image = status_icon(kIconRunning);
    button.toolTip = @"Kineo: click to pause, right-click for more";
    button.target = g_menu;
    button.action = @selector(clicked:);
    [button sendActionOn:NSEventMaskLeftMouseUp | NSEventMaskRightMouseUp];
    NSApp.delegate = g_menu;
}

void kn_set_paused(bool paused) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSStatusBarButton *button = g_menu.item.button;
        button.image = status_icon(paused ? kIconPaused : kIconRunning);
        button.toolTip = paused ? @"Kineo is paused: click to carry on, right-click for more"
                                : @"Kineo: click to pause, right-click for more";
    });
}

bool kn_wait_ax_trusted(double seconds) {
    if (AXIsProcessTrusted()) return true;
    // The Cocoa event loop isn't running yet, so pump it here: the menu
    // bar icon has to work while we wait.
    static dispatch_once_t once;
    dispatch_once(&once, ^{
        add_status_item();
        [NSApp finishLaunching];
        g_menu.item.button.image = status_icon(@"exclamationmark.triangle");
    });
    NSDate *until = [NSDate dateWithTimeIntervalSinceNow:seconds];
    while (until.timeIntervalSinceNow > 0) {
        NSEvent *e = [NSApp nextEventMatchingMask:NSEventMaskAny untilDate:until inMode:NSDefaultRunLoopMode dequeue:YES];
        if (e) [NSApp sendEvent:e];
    }
    if (!AXIsProcessTrusted()) return false;
    g_menu.item.button.image = status_icon(kIconRunning);
    return true;
}

// Lifecycle -------------------------------------------------------------------

bool kn_in_app_bundle(void) {
    return NSBundle.mainBundle.bundleIdentifier != nil;
}

bool kn_ax_trusted(bool prompt) {
    NSDictionary *opts = @{(__bridge NSString *)kAXTrustedCheckOptionPrompt : @(prompt)};
    return AXIsProcessTrustedWithOptions((__bridge CFDictionaryRef)opts);
}

void kn_init(void) {
    load_private_symbols();
    g_windows = [NSMutableDictionary new];
    g_apps = [NSMutableDictionary new];
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyAccessory];
}

void kn_run(kn_event_fn fn) {
    g_emit = fn;
    NSNotificationCenter *ws = NSWorkspace.sharedWorkspace.notificationCenter;
    NSOperationQueue *main = NSOperationQueue.mainQueue;
    NSRunningApplication * (^appOf)(NSNotification *) = ^(NSNotification *n) {
        return (NSRunningApplication *)n.userInfo[NSWorkspaceApplicationKey];
    };

    [ws addObserverForName:NSWorkspaceDidLaunchApplicationNotification object:nil queue:main
                usingBlock:^(NSNotification *n) { observe_app(appOf(n), 0); }];
    [ws addObserverForName:NSWorkspaceDidTerminateApplicationNotification object:nil queue:main
                usingBlock:^(NSNotification *n) {
                    pid_t pid = appOf(n).processIdentifier;
                    forget_app(pid);
                    emit(KN_APP_TERMINATED, pid, 0);
                }];
    [ws addObserverForName:NSWorkspaceDidHideApplicationNotification object:nil queue:main
                usingBlock:^(NSNotification *n) { emit(KN_APP_HIDDEN, appOf(n).processIdentifier, 0); }];
    [ws addObserverForName:NSWorkspaceDidUnhideApplicationNotification object:nil queue:main
                usingBlock:^(NSNotification *n) { emit(KN_APP_UNHIDDEN, appOf(n).processIdentifier, 0); }];
    [ws addObserverForName:NSWorkspaceDidActivateApplicationNotification object:nil queue:main
                usingBlock:^(NSNotification *n) { announce_focus(appOf(n).processIdentifier); }];
    [ws addObserverForName:NSWorkspaceActiveSpaceDidChangeNotification object:nil queue:main
                usingBlock:^(NSNotification *n) {
                    (void)n;
                    // Many apps only report windows on the active space, so
                    // look again for ones we have not seen.
                    for (KNApp *a in g_apps.allValues)
                        scan_app_windows(a.pid, (__bridge AXUIElementRef)a.element, true);
                    emit(KN_SPACE_CHANGED, 0, 0);
                }];
    [NSNotificationCenter.defaultCenter addObserverForName:NSApplicationDidChangeScreenParametersNotification
                                                    object:nil queue:main
                                                usingBlock:^(NSNotification *n) {
                                                    (void)n;
                                                    emit(KN_DISPLAYS_CHANGED, 0, 0);
                                                }];

    add_status_item();
    emit(KN_DISPLAYS_CHANGED, 0, 0);
    for (NSRunningApplication *running in NSWorkspace.sharedWorkspace.runningApplications) observe_app(running, 0);
    NSRunningApplication *front = NSWorkspace.sharedWorkspace.frontmostApplication;
    if (front) announce_focus(front.processIdentifier);

    [NSApp run];
}

void kn_quit(int code) {
    dispatch_async(dispatch_get_main_queue(), ^{ exit(code); });
}
