// The whole macOS surface Kineo uses, as a small plain-C API.
//
// Everything that needs Objective-C, CoreFoundation memory management or
// private frameworks lives behind this header, so the Haskell side only
// ever sees integers, doubles and flat structs.
#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef struct {
    double x, y, w, h;  // global coordinates, origin top-left of the primary display
} kn_rect;

typedef struct {
    int32_t pid;
    uint64_t space;  // 0 when unknown
    kn_rect frame;
    uint8_t standard;  // role AXWindow with subrole AXStandardWindow
    uint8_t resizable;
    uint8_t movable;
    uint8_t minimized;
    uint8_t fullscreen;
    char bundle_id[256];
    char title[512];
} kn_window_info;

typedef struct {
    uint32_t id;
    kn_rect frame;
    kn_rect visible;  // minus menu bar and Dock
    uint64_t space;
    uint8_t user_space;  // 0 for native full-screen spaces
} kn_display;

typedef struct {
    uint32_t keycode;
    uint32_t modifiers;  // Carbon modifier mask
} kn_hotkey;

// Event kinds passed to the event callback as (kind, pid, arg).
enum {
    KN_WINDOW_CREATED = 1,  // arg = window id
    KN_WINDOW_DESTROYED,
    KN_WINDOW_FOCUSED,
    KN_WINDOW_MOVED,
    KN_WINDOW_RESIZED,
    KN_WINDOW_MINIMIZED,
    KN_WINDOW_DEMINIMIZED,
    KN_APP_TERMINATED,  // arg unused
    KN_APP_HIDDEN,
    KN_APP_UNHIDDEN,
    KN_SPACE_CHANGED,     // pid and arg unused
    KN_DISPLAYS_CHANGED,  // pid and arg unused
    KN_HOTKEY,            // arg = index into the last kn_set_hotkeys array
};

// Called on the main thread. Must return quickly.
typedef void (*kn_event_fn)(int32_t kind, int32_t pid, uint32_t arg);

// Setup. kn_init and kn_run must be called on the main thread.
bool kn_ax_trusted(bool prompt);
void kn_init(void);
void kn_run(kn_event_fn fn);  // never returns
void kn_quit(int code);

// Queries and actions. Safe from any thread.
int kn_displays(kn_display *out, int max);
int kn_scan_windows(uint32_t *out, int max);
bool kn_query_window(uint32_t wid, kn_window_info *out);
bool kn_window_frame(uint32_t wid, kn_rect *out);
void kn_window_spaces(const uint32_t *wids, uint64_t *out, int n);
void kn_window_focus(uint32_t wid);
void kn_window_close(uint32_t wid);  // presses the close button
void kn_focus_nothing(void);         // makes Kineo, which has no windows, frontmost
void kn_set_hotkeys(const kn_hotkey *keys, int n);

// Flags for kn_window_set_frame.
enum { KN_SET_POSITION = 1, KN_SET_SIZE = 2 };
// Result codes for kn_window_set_frame.
enum { KN_OK = 0, KN_UNKNOWN_WINDOW = 1, KN_DEAD_WINDOW = 2, KN_FAILED = 3 };
int kn_window_set_frame(uint32_t wid, const kn_rect *r, int32_t what);
