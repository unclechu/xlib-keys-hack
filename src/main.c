/**
 * Author: Viacheslav Lotsmanov
 * License: GNU/GPLv3 https://raw.githubusercontent.com/unclechu/xlib-escape-key-hack/master/LICENSE
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>
#include <X11/keysym.h>

#define  APPNAME     "xlib-escape-key-hack"
#define  IDLE_TIME   10000
#define  KEYS_LIMIT  16

#define  CAPSKEY     66

Display *dpy;
Window   wnd;
unsigned int escape_key_code;

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

int main(const int argc, const char **argv)
{
	dpy = XOpenDisplay(NULL);
	wnd = DefaultRootWindow(dpy);
	
	escape_key_code = XKeysymToKeycode(dpy, XK_Escape);
	
	int caps_was_pressed = 0;
	int was_blocked = 0;
	
	char keys_return[KEYS_LIMIT];
	
	while (1) {
		
		int caps_is_pressed = 0;
		int another_key_is_pressed = 0;
		
		XQueryKeymap(dpy, keys_return);
		
		for (int i=0; i<KEYS_LIMIT; ++i) {
			
			if (keys_return[i] != 0) {
				
				int pos = 0;
				int num = keys_return[i];
				
				while (pos < 8) {
					
					if ((num & 0x01) == 1) {
						
						int key_num = i*8+pos;
						
						if (key_num == CAPSKEY) {
							caps_is_pressed = 1;
						} else {
							another_key_is_pressed = 1;
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
		
		usleep(IDLE_TIME); // idle
	}
	
	XCloseDisplay(dpy);
	return EXIT_SUCCESS;
}
