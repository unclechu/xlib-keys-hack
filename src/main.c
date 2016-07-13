/**
 * Author: Viacheslav Lotsmanov
 * License: GNU/GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>

#define  APPNAME      "xlib-keys-hack"
#define  IDLE_TIME    10000
#define  KEYS_LIMIT   16

#define  CAPS_KEY     66
#define  LALT_KEY     64
#define  RALT_KEY     108

#define  XMOBAR_PIPE  ".xmonad/xmobar.fifo"

Display *dpy;
Window   wnd;
unsigned int escape_key_code;
unsigned int level3_key_code;
int xmobar_pipe_is_open = 0;
char xmobar_pipe_abs_path[128];
int xmobar_pipe_fd = -1;
int is_numlock_on = 0;
int is_capslock_on = 0;
int is_level3_on = 0;

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

const char msg_numlock_on[] = "numlock:on\n";
const char msg_numlock_off[] = "numlock:off\n";
const char msg_capslock_on[] = "capslock:on\n";
const char msg_capslock_off[] = "capslock:off\n";
const char msg_level3_on[] = "level3:on\n";
const char msg_level3_off[] = "level3:off\n";
void detect_modes()
{
	XkbStateRec xkb_state;
	XkbGetState(dpy, XkbUseCoreKbd, &xkb_state);
	
	int cur_state;
	
	// numlock
	cur_state = (xkb_state.mods & 16) ? 1 : 0;
	if (cur_state != is_numlock_on) {
#ifdef DEBUG
		printf(
			"DEBUG: Writing 'numlock:%s' to XMobar PIPE...\n",
			(cur_state == 1) ? "on" : "off"
		);
#endif
		write(xmobar_pipe_fd, (
			(cur_state == 1) ? msg_numlock_on : msg_numlock_off
		), strlen(
			(cur_state == 1) ? msg_numlock_on : msg_numlock_off
		) + 1);
		is_numlock_on = cur_state;
	}
	
	// capslock
	cur_state = (xkb_state.mods & 2) ? 1 : 0;
	if (cur_state != is_capslock_on) {
#ifdef DEBUG
		printf(
			"DEBUG: Writing 'capslock:%s' to XMobar PIPE...\n",
			(cur_state == 1) ? "on" : "off"
		);
#endif
		write(xmobar_pipe_fd, (
			(cur_state == 1) ? msg_capslock_on : msg_capslock_off
		), strlen(
			(cur_state == 1) ? msg_capslock_on : msg_capslock_off
		) + 1);
		is_capslock_on = cur_state;
	}
	
	// level3
	cur_state = (xkb_state.mods & 128) ? 1 : 0;
	if (cur_state != is_level3_on) {
#ifdef DEBUG
		printf(
			"DEBUG: Writing 'level3:%s' to XMobar PIPE...\n",
			(cur_state == 1) ? "on" : "off"
		);
#endif
		write(xmobar_pipe_fd, (
			(cur_state == 1) ? msg_level3_on : msg_level3_off
		), strlen(
			(cur_state == 1) ? msg_level3_on : msg_level3_off
		) + 1);
		is_level3_on = cur_state;
	}
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
	
	char *home_dir = getenv("HOME");
	if (home_dir == NULL) {
		fprintf(stderr, "Can't get HOME directory path\n");
		return EXIT_FAILURE;
	}
	strcpy(xmobar_pipe_abs_path, home_dir);
	strcat(xmobar_pipe_abs_path, "/");
	strcat(xmobar_pipe_abs_path, XMOBAR_PIPE);
	
#ifdef DEBUG
	printf("DEBUG: XMobar PIPE file path: %s\n", xmobar_pipe_abs_path);
#endif
	
	struct stat xmobar_pipe_stat;
	if (stat(xmobar_pipe_abs_path, &xmobar_pipe_stat) == -1) {
		fprintf(stderr, "Get stat of XMobar PIPE error\n");
		return EXIT_FAILURE;
	}
	if ((xmobar_pipe_stat.st_mode & S_IFMT) == S_IFIFO) {
		xmobar_pipe_fd = open(xmobar_pipe_abs_path, O_WRONLY | O_NONBLOCK);
		if (xmobar_pipe_fd == -1) {
			fprintf(stderr, "Can't open XMobar PIPE for writing\n");
			return EXIT_FAILURE;
		}
#ifdef DEBUG
		printf("DEBUG: XMobar PIPE is opened to write\n");
#endif
	}
#ifdef DEBUG
	else {
		printf("DEBUG: XMobar PIPE file is not FIFO, won't open\n");
	}
#endif
	
	int caps_was_pressed = 0;
	int was_blocked = 0;
	int level3_is_active = 0;
	
#ifdef DEBUG
	int lalt_was_pressed = 0;
	int ralt_was_pressed = 0;
#endif
	
	char keys_return[KEYS_LIMIT];
	
	// reset previous press
	trigger_level3_release();
	if (xmobar_pipe_fd != -1) {
		detect_modes();
	}
	
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
						} else if (key_num != level3_key_code) {
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
		
		if (xmobar_pipe_fd != -1) {
			detect_modes();
		}
		usleep(IDLE_TIME); // idle
	}
	
	XCloseDisplay(dpy);
	return EXIT_SUCCESS;
}
