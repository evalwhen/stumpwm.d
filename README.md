# My StumpWM configuration

This repository contains my configuration for [StumpWM](https://stumpwm.github.io/). This configuration is, obviously, highly tailored to my own personal preferences and is likely to change (and break) over time. Therefore, I do **not** recommend to use this configuration as is yourself. Nonetheless, this configuration might serve as a source of inspiration for your own adventures with StumpWM.

## Screenshot

![Screenshot of StumpWM with running Emacs and Firefox](screenshot.png)

## Overview

To simplify maintenance of my configuration I split it into several files, each with a more or less clear responsibility. At the moment my configuration consists of the following files:

- `init.lisp` is the primary configuration file that binds everything together. It contains some core configuration, loads the necessary modules, and loads the remaining files listed below.
- `icons.lisp` and `applications.lisp` contain variables for respectively the icons used in the mode line and my commonly used applications.
- `colours.lisp` defines the colours used throughout this configuration. The colour scheme is based on the *Vivendi* variant of the [Modus themes](https://protesilaos.com/emacs/modus-themes) for Emacs.
- `util.lisp` defines several utility functions that I use throughout my configuration.
- `theme.lisp` contains the configuration for the visual aspects such as fonts and (border) colours.
- `placement.lisp` consists of configuration for groups creation and the preferred placement of frames with respect to these groups.
- `pamixer.lisp` is a wrapper for `pamixer` to control speaker and microphone volumes.
- `backlight.lisp` is a wrapper for `brightnessctl` to manage backlight brightness.
- `commands.lisp` defines a set of additional commands.
- `keybindings.lisp` defines a range of keybindings as well as undefines several default keybindings that I do not use.
- `mode-line.lisp` is the main configuration file for the mode line. It contains overall mode line settings as well as settings for several contrib modules. Furthermore, it loads the following additional files
  - `mode-line-windows.lisp` contains the functionality to group windows by class in the mode line instead of showing each window separately.
  - `mode-line-battery.lisp` is a small wrapper around the `battery-portable` module to vary what information is printed in the mode line depending on the battery state.
  - `mode-line-mail.lisp` specifies functions to periodically check for new email messages using `mu` and show an appropriate indicator in the mode line.
  - `mode-line-network.lisp` is a wrapper around contrib's wifi module to enrich the mode line with additional information such as whether a VPN connection is active.
  - `mode-line-updates.lisp` is a module to periodically check whether any package updates are available for my system and show this appropriately in the mode line.
  - `mode-line-disk.lisp` is a simple disk usage reporter that adds an indicator to the mode line once disk usage is higher than a given threshold.

## Used contrib modules

I use the following modules from [StumpWM contrib](https://github.com/stumpwm/stumpwm-contrib):

- `end-session` provides commands that allow the user to shutdown, restart, and logoff through the StumpWM UI.
- `mpd` provides a mode to display information about the [Music Player Daemon (MPD)](https://www.musicpd.org/)
- `battery-portable` allows to add battery information to the mode line.
- `cpu` allows to show CPU information in the mode line.
- `mem` allows to show memory information in the mode line.
- `wifi` allows to show WiFi information in the mode line.
- `swm-ssh` provides a simple menu selector to ssh to a remote host.
- `ttf-fonts` is used for TTF font rendering.

## Dependencies

In order to function properly my configuration relies on several software packages to be installed.

### Fonts

- [Hack font](https://sourcefoundry.org/hack/) is my primary font.
- [Font Awesome's](https://fontawesome.com/) free icons are used to spruce up the mode line.

### Tools

- [brightnessctl](https://github.com/Hummer12007/brightnessctl) used to control [backlight](backlight.lisp) brightness.
- [pamixer](https://github.com/cdemoulins/pamixer) and `pactl` to control audio devices from PulseAudio.
- [mu](https://www.djcbsoftware.nl/code/mu/) to check for new [email messages.](mode-line-mail.lisp)
- `checkupdates` utility from [pacman-contrib](https://gitlab.archlinux.org/pacman/pacman-contrib) and [paru](https://github.com/morganamilo/paru) to check for package updates.

Furthermore, I use some, more or less, common utilities throughout my configuration:

- [ripgrep](https://github.com/BurntSushi/ripgrep)
- `wc` from the [GNU core utilities](https://www.gnu.org/software/coreutils/)
- `ip` part of the iproute2 utilities

## Inspiration

This StumpWM configuration is partially inspired by and/or modified from others who made configurations available, such as:

- [alezost](https://github.com/alezost/stumpwm-config)
- [Phundrak](https://config.phundrak.com/stumpwm.html)

## Repositories

- Sourcehut: https://git.sr.ht/~onodrim/.stumpwm.d
- Mirror on Gitlab: https://gitlab.com/mirdono/dotstumpwm

<!-- Local Variables: -->
<!-- jinx-local-words: "PulseAudio Vivendi contrib contrib's wifi" -->
<!-- End: -->
