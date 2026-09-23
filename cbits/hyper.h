// Caps Lock as a hyper key (cmd+alt+ctrl), for kineo-hyper.
#pragma once

#include <stdbool.h>

// Does the process have Accessibility access? With prompt, macOS asks.
bool kh_trusted(bool prompt);

// Remap Caps Lock to F18 in the HID layer, merging with any existing
// UserKeyMapping. Remembers the previous mapping for kh_restore_mapping.
bool kh_install_mapping(void);
// Put back exactly the mapping that was there before.
void kh_restore_mapping(void);

// Create the event tap that turns F18 into held cmd+alt+ctrl. With
// escape, tapping Caps Lock on its own sends Escape. Returns false if
// the tap could not be created (usually: no Accessibility permission).
bool kh_start_tap(bool escape);

// Run the main run loop forever. Main thread only.
void kh_run(void);
