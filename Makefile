all: build

build:
	stack build --install-ghc
	stack install

dist: clean build

clean:
	stack clean --full
	rm -rf dist/ .stack-work/ .hpc/

test-command:
	sudo setfacl -m 'u:$(shell whoami):r' /dev/input/by-id/*

	stack build && \
		env PATH='$(shell stack path --local-install-root)/bin/:${PATH}' \
		xlib-keys-hack \
		\
		'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-if01' \
		'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-kbd' \
		'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-if01-event-kbd' \
		--disable-xinput-device-name='Corsair Corsair Gaming K63 Keyboard' \
		--disable-xinput-device-name='Corsair Corsair Gaming K63 Keyboard Keyboard' \
		\
		/dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd \
		/dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02 \
		--disable-xinput-device-name='2.4G Receiver' \
		\
		/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-kbd \
		/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-if01 \
		--disable-xinput-device-name='Apple Inc. Apple Keyboard' \
		\
		-v --shift-numeric-keys \
		--right-super-double-press-cmd='place-cursor-at' \
		--xmobar-indicators --software-debouncer=250
