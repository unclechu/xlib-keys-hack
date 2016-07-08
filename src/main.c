/**
 * Author: Viacheslav Lotsmanov
 * License: GNU/GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>
#include <X11/keysym.h>

#define  APPNAME     "xlib-keys-hack"
#define  IDLE_TIME   10000
#define  KEYS_LIMIT  16

#define  CAPS_KEY    66
#define  LALT_KEY    64
#define  RALT_KEY    108

Display *dpy;
Window   wnd;
unsigned int escape_key_code;
unsigned int level3_key_code;

void trigger_escape()
{
#ifdef DEBUG
	printf("DEBUG: Triggering escape key...\n");
#endif
	XTestFakeKeyEvent(dpy, escape_key_code, True, CurrentTime);
	XFlush(dpy);
	XTestFakeKeyEvent(dpy, escape_key_code, False, CurrentTime);
	XFlush(dpy);
}

void trigger_level3_press()
{
#ifdef DEBUG
	printf("DEBUG: Triggering Level3 modifier press...\n");
#endif
	XTestFakeKeyEvent(dpy, level3_key_code, True, CurrentTime);
	XFlush(dpy);
}

void trigger_level3_release()
{
#ifdef DEBUG
	printf("DEBUG: Triggering Level3 modifier release...\n");
#endif
	XTestFakeKeyEvent(dpy, level3_key_code, False, CurrentTime);
	XFlush(dpy);
}

int main(const int argc, const char **argv)
{
	dpy = XOpenDisplay(NULL);
	wnd = DefaultRootWindow(dpy);
	
	escape_key_code = XKeysymToKeycode(dpy, XK_Escape);
	level3_key_code = XKeysymToKeycode(dpy, XK_ISO_Level3_Shift);
	
#ifdef DEBUG
	printf("DEBUG: Escape key code: %d\n", escape_key_code);
	printf("DEBUG: Level3 key code: %d\n", level3_key_code);
#endif
	
	int caps_was_pressed = 0;
	int was_blocked = 0;
	int level3_is_active = 0;
	
#ifdef DEBUG
	int lalt_was_pressed = 0;
	int ralt_was_pressed = 0;
#endif
	
	char keys_return[KEYS_LIMIT];
	
	while (1) {
		
		int caps_is_pressed = 0;
		int another_key_is_pressed = 0;
		int lalt_is_pressed = 0;
		int ralt_is_pressed = 0;
		
		XQueryKeymap(dpy, keys_return);
		
		for (int i=0; i<KEYS_LIMIT; ++i) {
			
			if (keys_return[i] != 0) {
				
				int pos = 0;
				int num = keys_return[i];
				
				while (pos < 8) {
					
					if ((num & 0x01) == 1) {
						
						int key_num = i*8+pos;
						
						if (key_num == CAPS_KEY) {
							caps_is_pressed = 1;
						} else {
							another_key_is_pressed = 1;
						}
						
						if (key_num == LALT_KEY) {
							lalt_is_pressed = 1;
						}
						if (key_num == RALT_KEY) {
							ralt_is_pressed = 1;
						}
					}
					
					++pos;
					num /= 2;
				}
			}
		}
		
		if (was_blocked == 1 || another_key_is_pressed == 1) {
			
			caps_was_pressed = 0;
			was_blocked = 1;
			
			if (caps_is_pressed == 0 && another_key_is_pressed == 0) {
				was_blocked = 0;
			}
			
		} else if (caps_is_pressed == 1) {
#ifdef DEBUG
			if (caps_was_pressed == 0) {
				printf("DEBUG: Caps Lock is pressed\n");
			}
#endif
			caps_was_pressed = 1;
		} else if (caps_was_pressed == 1) {
			caps_was_pressed = 0;
			was_blocked = 1;
			trigger_escape();
		}
		
#ifdef DEBUG
		if (lalt_is_pressed == 1) {
			if (lalt_was_pressed == 0) {
				lalt_was_pressed = 1;
				printf("DEBUG: Left Alt is pressed\n");
			}
		} else {
			lalt_was_pressed = 0;
		}
		if (ralt_is_pressed == 1) {
			if (ralt_was_pressed == 0) {
				ralt_was_pressed = 1;
				printf("DEBUG: Right Alt is pressed\n");
			}
		} else {
			ralt_was_pressed = 0;
		}
#endif
		
		if (
			level3_is_active == 0 &&
			ralt_is_pressed == 0 &&
			caps_is_pressed == 1 &&
			lalt_is_pressed == 1
		) {
			level3_is_active = 1;
			trigger_level3_press();
		} else if (
			level3_is_active == 1 &&
			lalt_is_pressed == 0 &&
			ralt_is_pressed == 1
		) {
			level3_is_active = 0;
			trigger_level3_release();
		}
		
		usleep(IDLE_TIME); // idle
	}
	
	XCloseDisplay(dpy);
	return EXIT_SUCCESS;
}
