all: build

build:
	stack build --install-ghc
	stack install

dist: clean build

clean:
	rm -rf dist/ .cabal-sandbox/ cabal.sandbox.config .stack-work/

test-command:
	stack build && sudo \
		.stack-work/install/x86_64-linux/lts-6.27/7.10.3/bin/xlib-keys-hack \
		/dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd \
		/dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02 \
		/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-kbd \
		/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-if01 \
		--disable-xinput-device-name='2.4G Receiver' \
		--disable-xinput-device-name='Apple Inc. Apple Keyboard' \
		-v \
		--xmobar-pipe="${HOME}/.xmonad/xmobar.fifo"
