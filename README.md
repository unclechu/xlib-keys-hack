# xlib-keys-hack

[![Build Status](https://api.travis-ci.org/unclechu/xlib-keys-hack.svg?branch=master)](https://travis-ci.org/unclechu/xlib-keys-hack)

Keyboard behavior customization utility.

**WARNING!** Work in progress, not released yet, look at
[tasks board](https://github.com/unclechu/xlib-keys-hack/projects/1?fullscreen=true).

__**TODO**__ This description isn't complete yet,
             and some of it isn't true anymore.

- Simulation of pressing `Escape` key when press `Caps Lock` key without any combos;
- Simulation of pressing and holding third-level modificator by `Left Alt + Caps Lock`
  and simulation of releasing this key by `Right Alt`;
- Simulation of pressing real `Caps Lock` by `Caps Lock + Enter`
  (because `Caps Lock` used as additional `Left Ctrl`);
- Simulation of pressing real `Enter` by `Enter` key without any combos
  (because `Enter` key used as additional `Right Ctrl`);
- Pressing `Caps Lock` without any combos key that simulates `Escape` key
  also turns off caps lock mode, third-level mode
  and resets keyboard layout to first layout;
- Moving focus to another window resets keyboard layout to first layout,
  turns off caps lock mode and third-level mode;
- `Right Alt` works as resetter to all of modes (third-level shift, caps lock)
  and resets keyboard layout to first layout;
- Notifying my XMobar FIFO PIPE about third-level modificator, `Num Lock` and `Caps Lock`
  state to make indicators of these on panel
  ([see here for more details](https://github.com/unclechu/xmonadrc/blob/master/xmobar-cmd.sh)).

## Requirements

- [Haskell Tool Stack](https://haskellstack.org/)
- Development files of `libX11`, `libXtst` and `libXrandr`
- Development files of `libgmp` (for some dependencies)
- Superuser access (for giving to yourself read access permission (using ACL)
  to keyboard device file descriptor)

## Build and install

``` bash
$ make --always-make
```

Application binaries supposed to be installed by `stack` to `$HOME/.local/bin`
directory, make sure you have this directory in your `$PATH` environment variable.

## Run (as daemon)

1. For first you need to give read access permission to yourself (your user)
   to keyboard device file descriptor.

   You could easily do it for all input devices by this command:

   ``` bash
   $ sudo setfacl -m "u:$(whoami):r" /dev/input/by-id/*
   ```

   Keep in mind that after reboot or physically replug of your keyboard
   this permission will be reset (it's planned to describe solution about
   writing own systemd service to automate this).

2. For example you could have one keyboard and two file descriptors of device:

   ``` text
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02
   ```

   First for usual keys events and second for FN keys
   (like media keys, audio-player play/pause, increase/decrease volume, etc).

   You have to know your keyboard name in `xinput` to disable it,
   because this utility will read bare events behind X server and trigger
   specific fake X events, like simulating pressing keys, that's how it works.

   You could get all names of your `xinput` devices by this command:

   ``` bash
   $ xinput list
   ```

   Keyboard name in our example case is `2.4G Receiver`, let's check it:

   ``` bash
   $ xinput list | grep -F '2.4G Receiver'
   ```

   Will get us (if we really have this device):

   ``` text
   ↳ 2.4G Receiver  id=10  [slave  pointer  (2)]
   ↳ 2.4G Receiver  id=11  [slave  pointer  (2)]
   ↳ 2.4G Receiver  id=18  [slave  keyboard (3)]
   ↳ 2.4G Receiver  id=9   [slave  keyboard (3)]
   ```

   At this step we just found out our device `xinput` name that is:

   ``` text
   2.4G Receiver
   ```

   And our device file descriptors:

   ``` text
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02
   ```

3. Now you could start this utility daemon:

   ``` bash
   $ xlib-keys-hack -v \
        /dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd \
        /dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02 \
        --disable-xinput-device-name='2.4G Receiver'
   ```

   You could use more than one keyboard, here is an example:

   ``` bash
   $ xlib-keys-hack -v \
        /dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd \
        /dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02 \
        --disable-xinput-device-name='2.4G Receiver' \
        /dev/input/by-id/usb-04b4_6018-event-kbd \
        /dev/input/by-id/usb-04b4_6018-if01-event-mouse \
        /dev/input/by-id/usb-04b4_6018-if01-mouse \
        --disable-xinput-device-name='HID 04b4:6018'
   ```

   P.S. `-v` provides verbose debug information to stdout,
        you could remove it if you don't need this.

   P.S. Use `--help` to read about all possible options you could use.

   P.S. When this utility interrupted in terminal or terminated
        (`SIGINT` or `SIGTERM`) it enables `xinput` device back.
        So after you close it your keyboard supposed to keep working
        as before starting this utility.

4. Enjoy your hacked keyboard.

## More info

### --help

``` text
Usage: xlib-keys-hack [OPTION...] DEVICES-FD-PATHS...
  -h  --help                                                Show this usage info
  -v  --verbose                                             Start in verbose-mode
                                                            Default is: Off
      --real-capslock                                       Use real Caps Lock instead of remapping it to Escape
                                                            Default is: Off
      --no-additional-controls                              Disable additional controls behavior for Caps Lock and Enter keys
                                                            (could be comfortable for playing some video games)
                                                            Default is: On
      --shift-numeric-keys                                  Shift numeric keys in numbers row one key righter, and move 'minus' key to the left side at '1' key position.
                                                            Could be more consistent for 10-fingers typing.
                                                            Default is: Off
      --disable-toggling-alternative-mode-by-alts           Disable toggling alternative mode by pressing Alt keys (Left and Right) both at the same time
                                                            Default is: On
      --disable-super-double-press                          Disable handling of double Super key press.
                                                            Default is: On
      --super-double-press-cmd=COMMAND                      When Super key is pressed twice in short interval alternative mode will be toggled or specified shell command will be spawned.
                                                            This option makes no sense with --disable-super-double-press
      --left-super-double-press-cmd=COMMAND                 Double Left Super key press will spawn specified shell command instead of toggling alternative mode.
                                                            This option makes no sense with --disable-super-double-press
      --right-super-double-press-cmd=COMMAND                Double Right Super key press will spawn specified shell command instead of toggling alternative mode.
                                                            This option makes no sense with --disable-super-double-press
      --disable-reset-by-escape-on-capslock                 Disable resetting Caps Lock mode, Alternative mode and keyboard layout by Escape that triggered by Caps Lock key
                                                            (only when it's remapped, no need to use this option if you already use --real-capslock)
                                                            Default is: On
      --disable-reset-by-window-focus-event                 Disable resetting Caps Lock mode, Alternative mode and keyboard layout by switching between windows.
                                                            WARNING! If you don't disable this feature you should ensure that you have directory that contains 'xlib-keys-hack-watch-for-window-focus-events' executable in your 'PATH' environment variable!
                                                            Default is: On
      --disable-xinput-device-name[=NAME]                   Name of device to disable using 'xinput' tool
      --disable-xinput-device-id[=ID]                       Id of device to disable using 'xinput' tool
      --device-fd-path[=FDPATH]                             Path to device file descriptor to get events from
      --xmobar-indicators                                   Enable notifying xmobar indicators process about indicators (num lock, caps lock and alternative mode) state changes by DBus.
                                                            See also https://github.com/unclechu/xmonadrc
                                                            Default is: Off
      --xmobar-indicators-dbus-path[=PATH]                  DBus object path for xmobar indicators.
                                                            Default is: '/'
                                                            This option makes sense only with --xmobar-indicators
      --xmobar-indicators-dbus-bus[=BUS]                    DBus bus name for xmobar indicators.
                                                            Default is: 'com.github.unclechu.xmonadrc.%DISPLAY%' where '%DISPLAY%' is view of $DISPLAY environment variable where ':' and '.' symbols are replaced to underscore '_', for example if we have $DISPLAY as ':1' bus name will be 'com.github.unclechu.xmonadrc._1'
                                                            This option makes sense only with --xmobar-indicators
                                                            Use --xmobar-indicators-dbus-bus=any to broadcast to everyone.
      --xmobar-indicators-dbus-interface[=INTERFACE]        DBus interface for xmobar indicators.
                                                            Default is: 'com.github.unclechu.xmonadrc'
                                                            This option makes sense only with --xmobar-indicators
      --xmobar-indicators-dbus-flush-path[=PATH]            DBus object path for 'flush' request from xmobar indicators process.
                                                            Default is: '/com/github/unclechu/xmonadrc/%DISPLAY%' where '%DISPLAY%' is view of $DISPLAY environment variable where ':' and '.' symbols are replaced to underscore '_', for example if we have $DISPLAY as ':1' object path will be '/com/github/unclechu/xmonadrc/_1'
                                                            This option makes sense only with --xmobar-indicators
      --xmobar-indicators-dbus-flush-interface[=INTERFACE]  DBus interface for 'flush' request from xmobar indicators process.
                                                            Default is: 'com.github.unclechu.xmonadrc'
                                                            This option makes sense only with --xmobar-indicators
```

### Generating coverage report

``` bash
$ stack clean
$ stack test --ghc-options=-fhpc --coverage
```

## Author

[Viacheslav Lotsmanov](https://github.com/unclechu)

## License

[GNU/GPLv3](./LICENSE)
