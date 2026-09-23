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
}
```

Kineo needs **Accessibility** access (System Settings → Privacy & Security →
Accessibility). macOS grants it per binary, so after installing a new build
from Nix you may need to remove and re-add it. Turn on **Displays have
separate Spaces** (System Settings → Desktop & Dock) for multiple displays.

`kineo doctor` shows what Kineo can see (permission, displays, spaces, and
which windows it would tile) without moving anything.

## Use

Default bindings (hyper = cmd+alt+ctrl):

| Keys | Command |
| --- | --- |
| hyper + a / d / w / s | focus left / right / up / down |
| hyper + f / e | focus first / last |
| hyper + shift + a / d / w / s | move window left / right / up / down |
| hyper + t, hyper + shift + t | cycle column width forward / back |
| hyper + m | toggle full width |
| hyper + c | centre the focused column |
| hyper + comma / period | stack into the left column / unstack |
| hyper + space | float or tile the focused window |
| hyper + r | retile everything |
| hyper + shift + r | reload the config |
| hyper + shift + q | quit (parked windows come back on screen) |

Everything is configurable: copy [`config/kineo.toml`](config/kineo.toml) to
`~/.config/kineo/kineo.toml`. Unknown keys are reported rather than ignored;
`kineo check-config` validates a file.

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
