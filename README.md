# xlib-keys-hack

__**TODO**__ Replace Travis shield branch to `master` after first release.<br>
`haskell-version` branch: [![Build Status](https://api.travis-ci.org/unclechu/xlib-keys-hack.svg?branch=haskell-version)](https://travis-ci.org/unclechu/xlib-keys-hack)

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

```bash
$ make --always-make
```

Application binaries supposed to be installed by `stack` to `$HOME/.local/bin`
directory, make sure you have this directory in your `$PATH` environment variable.

## Run (as daemon)

1. For first you need to give read access permission to yourself (your user)
   to keyboard device file descriptor.

   You could easily do it for all input devices by this command:

   ```bash
   $ sudo setfacl -m 'u:$(whoami):r' /dev/input/by-id/*
   ```

   Keep in mind that after reboot or physically replug of your keyboard
   this permission will be reset (it's planned to describe solution to
   write some systemd service to automate this).

2. For example you could have one keyboard and two file descriptors of device:

   ```text
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02
   ```

   First for usual keys events and second for FN keys
   (like media keys, audio-player play/pause, increase/decrease volume, etc).

   And you need to know your keyboard name in `xinput` to disable it,
   because this utility will read bare events behind X server and trigger
   specific fake X events, like simulating pressing keys, that's how it works.

   You could get all names of your `xinput` devices by this command:

   ```bash
   $ xinput list
   ```

   Keyboard name in our example case is `2.4G Receiver`, let's check it:

   ```bash
   $ xinput list | grep -F '2.4G Receiver'
   ```

   Will get us (if we really have this device):

   ```text
   ⎜   ↳ 2.4G Receiver                             id=10   [slave  pointer  (2)]
   ⎜   ↳ 2.4G Receiver                             id=11   [slave  pointer  (2)]
       ↳ 2.4G Receiver                             id=18   [slave  keyboard (3)]
       ↳ 2.4G Receiver                             id=9    [slave  keyboard (3)]
   ```

   At this step we just found out our device `xinput` name that is:

   ```text
   2.4G Receiver
   ```

   And our device file descriptors:

   ```text
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd
   /dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02
   ```

3. Now you could start this utility daemon:

   ```bash
   xlib-keys-hack -v \
     /dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd \
     /dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02 \
     --disable-xinput-device-name='2.4G Receiver'
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

### Generating coverage report

``` bash
$ stack clean
$ stack test --ghc-options=-fhpc --coverage
```

## Author

[Viacheslav Lotsmanov](https://github.com/unclechu)

## License

[GNU/GPLv3](./LICENSE)
