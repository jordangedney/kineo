# Kineo

*κινέω, "I move".* A scrolling tiling window manager for macOS.

Windows sit side by side on an endless horizontal strip. Opening a window
never resizes the ones you already have: the new window takes its own column
and the strip scrolls to show whatever has focus. Windows that scroll off
screen park at the edge of the display with a sliver showing.

```
      ┌──────────── display ────────────┐
 ▐ …  │ ┌────────┐ ┌────────┐ ┌───────┐ │ ┌──────┐ …▌
 ▐    │ │ editor │ │ browser│ │ term  │ │ │ chat │  ▌
 ▐    │ │        │ │        │ ├───────┤ │ │      │  ▌
 ▐    │ │        │ │        │ │ term  │ │ │      │  ▌
      │ └────────┘ └────────┘ └───────┘ │ └──────┘
      └─────────────────────────────────┘
```

## Try it

Kineo and [kineo-hyper](#caps-lock-as-hyper) together, with debug logs.
Quitting Kineo (hyper + shift + q, or ctrl-c) stops both and gives Caps
Lock back:

```sh
nix run 'git+ssh://git@github.com/jordangedney/kineo#dev'
nix run .#dev        # the same, from a clone
```

## Install

```sh
nix run github:jordangedney/kineo          # try it
nix profile install github:jordangedney/kineo
```

or with nix-darwin, as a launchd agent:

```nix
{
  inputs.kineo.url = "github:jordangedney/kineo";
  # ...
  imports = [ inputs.kineo.darwinModules.default ];
  services.kineo.enable = true;
  # services.kineo.settings = builtins.readFile ./kineo.toml;
  services.kineo.hyper.enable = true;   # Caps Lock as hyper (see below)
  # services.kineo.hyper.escape = true; # ...and tapped alone, Escape
}
```

Kineo needs **Accessibility** access (System Settings → Privacy & Security →
Accessibility). macOS grants it per binary, so after installing a new build
from Nix you may need to remove and re-add it. Turn on **Displays have
separate Spaces** (System Settings → Desktop & Dock) for multiple displays.

`kineo doctor` shows what Kineo can see (permission, displays, spaces, and
which windows it would tile) without moving anything.

### Caps Lock as hyper

`kineo-hyper` turns Caps Lock into hyper (cmd+alt+ctrl) while it runs:
hold Caps Lock and press a key, and every app sees cmd+alt+ctrl+key. With
`--escape`, tapping Caps Lock on its own sends Escape. It is a separate
process from the window manager, so restarting Kineo never takes your
keyboard with it, and it needs its own Accessibility grant.

It remaps Caps Lock to F18 in the HID layer (like `hidutil`) and puts back
whatever mapping was there before when it exits. If it is killed with
`kill -9`, Caps Lock does nothing until you run it again or restart
(`hidutil property --set '{"UserKeyMapping":[]}'` also clears it).

## Use

Default bindings (hyper = cmd+alt+ctrl):

| Keys | Command |
| --- | --- |
| hyper + a / d / w / s | focus left / right / up / down |
| hyper + h / l | focus first / last |
| hyper + shift + a / d / w / s | move window left / right / up / down |
| hyper + c, hyper + shift + c | cycle column width forward / back |
| hyper + f | toggle full width |
| hyper + m | centre (middle) the focused column |
| hyper + comma / period | stack into the left column / unstack |
| hyper + p | pop the focused window out to float, or back in to tile |
| hyper + return | new iTerm window |
| hyper + delete | close the focused window |
| hyper + r | retile everything |
| hyper + shift + r | reload the config |
| hyper + shift + q | quit (parked windows come back on screen) |

### Workspaces

Each macOS space holds a vertical stack of workspaces, each with its own
strip. Only one is on screen; the others park below the display with a
sliver showing. (Not above: macOS keeps windows below the menu bar.)

- **Focus up or down** moves within a stacked column first. At the top or
  bottom of the column it goes to the workspace above or below. Past the
  first or last workspace it goes to a new, empty one; windows opened
  there stay there. On an empty workspace no window has keyboard focus.
- **Move up or down** works the same way and takes the window along,
  starting a new workspace past either end.
- **An empty workspace** disappears once you leave it. When the one on
  screen empties because its last window closed, the workspace above it
  (or else below) takes its place.
- **Focusing a parked window** (with cmd-tab, or a click on its sliver)
  brings its workspace on screen.

Everything is configurable: copy [`config/kineo.toml`](config/kineo.toml) to
`~/.config/kineo/kineo.toml`. Unknown keys are reported rather than ignored;
`kineo check-config` validates a file.

A binding can also run a shell command: `"hyper+b" = "exec open -a Safari"`.
The iTerm binding uses a profile named "Kineo" when there is one: copy
[`config/iterm-profile.json`](config/iterm-profile.json) to
`~/Library/Application Support/iTerm2/DynamicProfiles/` for windows without
a title bar. The first time, macOS may ask to let Kineo control iTerm.

### Driving Kineo from outside

A running Kineo listens on a Unix socket, one command name per line:

```sh
kineo send focus-left
kineo send "cycle width"      # case, spaces and underscores are forgiven
kineo commands                # list them all
```

This is the hook for scripts, launchers and voice control: anything that
can produce a command name can move your windows.

## Develop

```sh
nix develop          # GHC 9.12, cabal, HLS, fourmolu
cabal test           # pure core: unit and property tests
cabal run kineo -- doctor
nix build            # the release binary, tests included
nix run .#dev        # kineo (debug logs) + kineo-hyper; quitting stops both
```

The design keeps all the window-management logic pure:

| | |
| --- | --- |
| `src/Kineo/Strip.hs` | the strip: columns of stacked windows |
| `src/Kineo/Layout.hs` | columns → screen rectangles, scrolling, edge parking |
| `src/Kineo/Core.hs` | `step :: Config -> Event -> World -> (World, [Effect])` |
| `src/Kineo/Config.hs`, `Keys.hs`, `Command.hs` | TOML config, key chords, command names |
| `app/Kineo/Runtime.hs` | queue → sense → step → perform |
| `app/Kineo/Animator.hs` | smooth moves on their own thread |
| `app/Kineo/Platform.hs`, `cbits/kineo.m` | the macOS layer, a small C API over AppKit/Accessibility |

The library has no macOS dependencies, so `CoreSpec`'s property tests can
throw thousands of random event sequences at the window manager and check
that no window is ever lost, duplicated or placed twice.
