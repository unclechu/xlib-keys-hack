all: build

build:
	stack build --install-ghc
	stack install

dist: clean build

clean:
	rm -rf dist/ .stack-work/ .hpc/

test-command:
	sudo setfacl -m 'u:$(shell whoami):r' /dev/input/by-id/*

	stack build && \
		env PATH='$(shell stack path --local-install-root)/bin/:${PATH}' \
		xlib-keys-hack \
		/dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd \
		/dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02 \
		/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-kbd \
		/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-if01 \
		--disable-xinput-device-name='2.4G Receiver' \
		--disable-xinput-device-name='Apple Inc. Apple Keyboard' \
		-v \
		--xmobar-indicators
