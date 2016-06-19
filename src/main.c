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

#define  APPNAME     "xlib-escape-key-hack"
#define  IDLE_TIME   10000
#define  KEYS_LIMIT  16

#define  LSHIFT      50
#define  RSHIFT      62

void trigger_escape()
{
	const pid_t child_pid = fork();
	
	if (child_pid < 0) {
		fprintf(stderr, "Can't fork.\n");
		perror("fork");
		exit(EXIT_FAILURE);
	}
	
	// parent process
	if (child_pid != 0) {
		return;
	}
	
#ifdef DEBUG
	printf("DEBUG: Triggering escape key...\n");
#endif
	if (execlp("xdotool", "xdotool", "key", "Escape", NULL) == -1) {
		fprintf(stderr, "xdotool execution error.\n");
		perror("execlp");
		exit(EXIT_FAILURE);
	}
}

int main(const int argc, const char **argv)
{
	Display *dpy = XOpenDisplay(NULL);
	Window   wnd = DefaultRootWindow(dpy);
	
	int lshift_was_pressed = 0;
	int rshift_was_pressed = 0;
	int was_blocked = 0;
	
	char keys_return[KEYS_LIMIT];
	
	while (1) {
		
		int lshift_is_pressed = 0;
		int rshift_is_pressed = 0;
		int another_key_is_pressed = 0;
		
		XQueryKeymap(dpy, keys_return);
		
		for (int i=0; i<KEYS_LIMIT; ++i) {
			
			if (keys_return[i] != 0) {
				
				int pos = 0;
				int num = keys_return[i];
				
				while (pos < 8) {
					
					if ((num & 0x01) == 1) {
						
						int key_num = i*8+pos;
						
						if (key_num == LSHIFT) {
							lshift_is_pressed = 1;
						} else if (key_num == RSHIFT) {
							rshift_is_pressed = 1;
						} else {
							another_key_is_pressed = 1;
						}
					}
					
					++pos;
					num /= 2;
				}
			}
		}
		
		if (
			was_blocked == 1 ||
			another_key_is_pressed == 1 ||
			
			// both shifts prevents escape key triggering
			(lshift_is_pressed == 1 && rshift_is_pressed == 1) ||
			(lshift_was_pressed == 1 && rshift_is_pressed == 1) ||
			(lshift_is_pressed == 1 && rshift_was_pressed == 1) ||
			(lshift_was_pressed == 1 && rshift_was_pressed == 1)
		) {
			
			lshift_was_pressed = 0;
			rshift_was_pressed = 0;
			was_blocked = 1;
			
			if (
				lshift_is_pressed == 0 &&
				rshift_is_pressed == 0 &&
				another_key_is_pressed == 0
			) {
				was_blocked = 0;
			}
			
		} else if (lshift_is_pressed == 1) {
			lshift_was_pressed = 1;
		} else if (rshift_is_pressed == 1) {
			rshift_was_pressed = 1;
		} else if (lshift_was_pressed == 1 || rshift_was_pressed == 1) {
			lshift_was_pressed = 0;
			rshift_was_pressed = 0;
			was_blocked = 1;
			trigger_escape();
		}
		
		usleep(IDLE_TIME); // idle
	}
	
	XCloseDisplay(dpy);
	return EXIT_SUCCESS;
}
