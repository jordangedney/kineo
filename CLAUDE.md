# CLAUDE.md

Kineo is a scrolling tiling window manager for macOS, in Haskell (GHC 9.12,
`GHC2024`) built with Nix. It replaces the earlier `paneru-hs` port.

## Commands

```sh
nix develop                      # toolchain: ghc 9.12, cabal, HLS, fourmolu
cabal build                      # library + executable
cabal test                       # pure test suite (no macOS permissions needed)
cabal test --test-options='--quickcheck-tests=5000'
cabal run kineo -- doctor        # read-only: permissions, displays, windows
cabal run kineo -- check-config config/kineo.toml
nix build                        # release binary; runs the tests too
```

`nix build`/`nix develop` only see files tracked by git: `git add` new files
first. Don't run `kineo` with no arguments as a check: it takes over window
management and rearranges every window on screen. Use `doctor` instead.

## Architecture

The library (`src/`) is pure and has no macOS dependencies; everything that
touches the OS is in the executable (`app/`, `cbits/`).

- `Kineo.Strip`: columns of vertically stacked windows. Total functions;
  unknown windows leave the strip unchanged.
- `Kineo.Layout`: strip + scroll → screen rects. Column widths are fractions
  of the usable width (`columnPx`); a space's `scroll` is in strip pixels.
  Off-screen windows are parked at the display edge with a `sliver` visible,
  and never overlap a neighbouring display.
- `Kineo.Core`: `step :: Config -> Event -> World -> (World, [Effect])`.
  All window-management policy lives here. A macOS space holds a vertical
  stack of `Workspace`s (strip, scroll, last focus); only the active one is
  laid out, the rest are `stow`ed below the display. Focus/move up and down
  cross workspaces at the end of a column, and focus past either end goes
  to a new empty workspace (`World.blankFocus`; Kineo activates itself so
  no parked window keeps the keyboard). `tidy` drops empty inactive
  workspaces and leaves an active one whose windows all went away, unless
  the user went there empty on purpose. Windows can't be parked above the
  screen: macOS clamps them below the menu bar. `exec` commands run a
  shell command and are refused over the socket. Invariants
  (property-tested in `test/CoreSpec.hs`, checked in `test/Gen.hs`): every
  non-floating tracked window is in exactly one strip, in a workspace of
  its space; a space has at least one workspace and no empty inactive one.
- `Kineo.Config` / `Keys` / `Command`: TOML via `toml-parser`, chords like
  `hyper+shift+a` (hyper = cmd+alt+ctrl), and stable kebab-case command
  names shared by key bindings and `kineo send`.
- `app/Kineo/Runtime.hs`: main thread runs Cocoa (`Platform.runLoop`); C
  callbacks only enqueue. One worker thread owns the `World`: `sense`
  (IO queries, turn raw events into `Event`s) → `step` → `perform`.
- `app/Kineo/Animator.hs`: its own thread; blocks in STM when idle. Sets the
  final size on the first frame and animates position only.
- `app/Kineo/Remote.hs`: Unix socket `/tmp/kineo-<uid>.sock`, one command
  name per line. The intended hook for voice control.
- `cbits/kineo.{h,m}`: the entire macOS surface as a plain-C API. ARC, and
  private symbols (SkyLight, `_AXUIElementGetWindow`) are resolved with
  `dlsym`. `app/Kineo/Platform/FFI.hsc` binds it with hsc2hs offsets.
- `kineo-hyper` (`hyper/Main.hs`, `cbits/hyper.m`): a separate executable,
  on purpose, so the keyboard never depends on the window manager. It maps
  Caps Lock → F18 via `IOHIDEventSystemClient` "UserKeyMapping" (restored
  on exit), and an HID-level event tap adds cmd+alt+ctrl to key events while
  F18 is held. The tap callback must stay pure C and fast; macOS disables
  slow taps (the callback re-enables on `kCGEventTapDisabledByTimeout`).
  Don't test it by injecting key events: they reach the frontmost app.

## Conventions

- Records use `OverloadedRecordDot` with `NoFieldSelectors` and
  `DuplicateRecordFields`. GHC 9.12 rejects record *updates* on a field name
  shared by two types in scope, so give fields distinct names when both
  types meet (e.g. `Column.stack` vs `World.windows`).
- New behaviour goes in `Core` with a test; the platform layer should only
  translate.
- C functions that touch `g_apps` or AppKit run on the main thread; the
  window table is shared under `@synchronized(g_windows)`.
