// Caps Lock as a hyper key (cmd+alt+ctrl), for kineo-hyper and Kineo.app.
#pragma once

#include <stdbool.h>

// Does the process have Accessibility access? With prompt, macOS asks.
bool kh_trusted(bool prompt);

// Turn the hyper key on or off, or change whether a tap sends Escape. Safe
// to call again with new settings. On: remaps Caps Lock to F18 in the HID
// layer (merging with any existing UserKeyMapping) and starts an event tap,
// on its own thread, that turns a held F18 into cmd+alt+ctrl. Off, or when
// the process exits: puts back the mapping that was there before. Returns
// false if the tap could not be created (usually: no Accessibility access).
bool kh_configure(bool on, bool escape);
