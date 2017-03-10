# xlib-keys-hack

__**TODO**__ Replace Travis shield branch to `master` after first release.<br>
`haskell-version` branch: [![Build Status](https://api.travis-ci.org/unclechu/xlib-keys-hack.svg?branch=haskell-version)](https://travis-ci.org/unclechu/xlib-keys-hack)

Keyboard behavior customization utility.

__**TODO**__
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
- Superuser access (for handling keyboard device file descriptor)

## Build and install (to `$HOME` directory)

```bash
$ make --always-make
```

## Run (as daemon)

__**TODO**__

## More info

### Generating coverage report

``` bash
$ stack test --ghc-options=-fhpc --coverage
```

## Author

[Viacheslav Lotsmanov](https://github.com/unclechu)

## License

[GNU/GPLv3](./LICENSE)
