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

#include <pthread.h>


#define  APPNAME      "xlib-keys-hack"
#define  IDLE_TIME    10000
#define  KEYS_LIMIT   16


#define  CAPS_KEY     66
#define  ENTER_KEY    36
#define  LCTRL_KEY    37
#define  RCTRL_KEY    105

#define  LALT_KEY     64
#define  RALT_KEY     108

#define  LSHIFT_KEY   50
#define  RSHIFT_KEY   62


#define  XMOBAR_PIPE  ".xmonad/xmobar.fifo"


Display *dpy;
Window   wnd;
unsigned int escape_key_code;
unsigned int level3_key_code;
unsigned int capslock_key_code;
unsigned int enter_key_code;
int xmobar_pipe_is_open = 0;
char xmobar_pipe_abs_path[128];
int xmobar_pipe_fd = -1;
int is_numlock_on = 0;
int is_capslock_on = 0;
int is_level3_on = 0;

Display *window_focus__dpy;
Window window_focus__wnd;
XEvent window_focus__event;
int window_focus__revert_to;
int window_focus__last_wnd_id = 0;

inline static void trigger_escape()
{
#ifdef DEBUG
	printf("DEBUG: Triggering Escape key...\n");
#endif
	XTestFakeKeyEvent(dpy, escape_key_code, True, CurrentTime);
	XFlush(dpy);
	XTestFakeKeyEvent(dpy, escape_key_code, False, CurrentTime);
	XFlush(dpy);
}

inline static void trigger_enter()
{
#ifdef DEBUG
	printf("DEBUG: Triggering Enter key...\n");
#endif
	XTestFakeKeyEvent(dpy, enter_key_code, True, CurrentTime);
	XFlush(dpy);
	XTestFakeKeyEvent(dpy, enter_key_code, False, CurrentTime);
	XFlush(dpy);
}

inline static void trigger_level3_press()
{
#ifdef DEBUG
	printf("DEBUG: Triggering Level3 modifier press...\n");
#endif
	XTestFakeKeyEvent(dpy, level3_key_code, True, CurrentTime);
	XFlush(dpy);
}

inline static void trigger_level3_release()
{
#ifdef DEBUG
	printf("DEBUG: Triggering Level3 modifier release...\n");
#endif
	XTestFakeKeyEvent(dpy, level3_key_code, False, CurrentTime);
	XFlush(dpy);
}

inline static void trigger_capslock()
{
#ifdef DEBUG
	printf("DEBUG: Triggering Caps Lock key...\n");
#endif
	XTestFakeKeyEvent(dpy, capslock_key_code, True, CurrentTime);
	XFlush(dpy);
	XTestFakeKeyEvent(dpy, capslock_key_code, False, CurrentTime);
	XFlush(dpy);
}

inline static void flush_modes()
{
#ifdef DEBUG
	printf("DEBUG: Flushing modes...\n");
#endif
	if (is_capslock_on == 1) {
		trigger_capslock();
	}
	if (is_level3_on == 1) {
		trigger_level3_release();
	}
}

inline static void reset_xkb_layout()
{
#ifdef DEBUG
	printf("DEBUG: Resetting keyboard layout...\n");
#endif
	XkbLockGroup(dpy, XkbUseCoreKbd, 0); // change to first layout
}

inline static void reset_everything()
{
#ifdef DEBUG
	printf("DEBUG: Resetting everything...\n");
#endif
	reset_xkb_layout();
	flush_modes();
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
		if (xmobar_pipe_fd != -1) {
			write(xmobar_pipe_fd, (
				(cur_state == 1) ? msg_numlock_on : msg_numlock_off
			), strlen(
				(cur_state == 1) ? msg_numlock_on : msg_numlock_off
			) + 1);
		}
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
		if (xmobar_pipe_fd != -1) {
			write(xmobar_pipe_fd, (
				(cur_state == 1) ? msg_capslock_on : msg_capslock_off
			), strlen(
				(cur_state == 1) ? msg_capslock_on : msg_capslock_off
			) + 1);
		}
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
		if (xmobar_pipe_fd != -1) {
			write(xmobar_pipe_fd, (
				(cur_state == 1) ? msg_level3_on : msg_level3_off
			), strlen(
				(cur_state == 1) ? msg_level3_on : msg_level3_off
			) + 1);
		}
		is_level3_on = cur_state;
	}
}

void *window_focus__thread_handler(void *ptr)
{
	while (1) {
		XGetInputFocus(
			window_focus__dpy, &window_focus__wnd, &window_focus__revert_to
		);
		XSelectInput(window_focus__dpy, window_focus__wnd, FocusChangeMask);
		XNextEvent(window_focus__dpy, &window_focus__event);
		
		// check for new window focus
		if ((int)window_focus__wnd != window_focus__last_wnd_id) {
#ifdef DEBUG
			printf("DEBUG: Window focus was moved...\n");
#endif
			reset_everything();
			window_focus__last_wnd_id = (int)window_focus__wnd;
		}
	}
}

int window_focus__error_handler(Display *dpy, XErrorEvent *ev)
{
	// we get BadWindow error when close focused window
	// it's okay
	if (ev->error_code == 3) {
#ifdef DEBUG
		printf("DEBUG: BadWindow error successfully handled\n");
#endif
		return 0;
	}
	
	char mess[128];
	XGetErrorText(dpy, ev->error_code, mess, sizeof(mess));
	fprintf(stderr, "X11 Error: %s (error code: %d)\n", mess, ev->error_code);
	exit(ev->error_code);
	return ev->error_code;
}

void window_focus__display_init()
{
	int reason_return;
	
	window_focus__dpy = XkbOpenDisplay(
		NULL, NULL, NULL, NULL, NULL, &reason_return
	);
	
	switch (reason_return) {
		case XkbOD_BadLibraryVersion:
			fprintf(stderr, "Bad Xkb library version\n");
			exit(EXIT_FAILURE);
			break;
		case XkbOD_ConnectionRefused:
			fprintf(stderr, "Connection to X server refused\n");
			exit(EXIT_FAILURE);
			break;
		case XkbOD_BadServerVersion:
			fprintf(stderr, "Bad X11 server version\n");
			exit(EXIT_FAILURE);
			break;
		case XkbOD_NonXkbServer:
			fprintf(stderr, "Xkb not present\n");
			exit(EXIT_FAILURE);
			break;
		case XkbOD_Success:
			// move forward
			break;
		default:
			fprintf(stderr, "Xkb error with unknown reason\n");
			exit(EXIT_FAILURE);
	}
	
	XSetErrorHandler(window_focus__error_handler);
}

int main(const int argc, const char **argv)
{
	dpy = XOpenDisplay(NULL);
	wnd = DefaultRootWindow(dpy);
	
	escape_key_code = XKeysymToKeycode(dpy, XK_Escape);
	level3_key_code = XKeysymToKeycode(dpy, XK_ISO_Level3_Shift);
	capslock_key_code = XKeysymToKeycode(dpy, XK_Caps_Lock);
	enter_key_code = XKeysymToKeycode(dpy, XK_Return);
	
#ifdef DEBUG
	printf("DEBUG: Escape key code: %d\n", escape_key_code);
	printf("DEBUG: Level3 key code: %d\n", level3_key_code);
	printf("DEBUG: Caps Lock key code: %d\n", capslock_key_code);
	printf("DEBUG: Enter key code: %d\n", enter_key_code);
#endif
	
	XkbDescRec* kbd_desc_ptr = XkbAllocKeyboard();
	if (kbd_desc_ptr == NULL) {
		fprintf(stderr, "Xkb init error (XkbAllocKeyboard)\n");
		return EXIT_FAILURE;
	}
	XkbGetControls(dpy, XkbAllControlsMask, kbd_desc_ptr);
	if (kbd_desc_ptr->ctrls->num_groups <= 0) {
		fprintf(stderr, "Xkb init error (XkbGetControls)\n");
		return EXIT_FAILURE;
	}
	
	window_focus__display_init();
	
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
	
	int xmobar_pipe_exists = open(xmobar_pipe_abs_path, O_RDONLY | O_NONBLOCK);
	if (xmobar_pipe_exists != -1) {
		close(xmobar_pipe_exists);
		struct stat xmobar_pipe_stat;
		if (stat(xmobar_pipe_abs_path, &xmobar_pipe_stat) == -1) {
			fprintf(stderr, "Get stat of XMobar PIPE error\n");
			return EXIT_FAILURE;
		}
		if (S_ISFIFO(xmobar_pipe_stat.st_mode)) {
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
	}
#ifdef DEBUG
	else {
		printf("DEBUG: XMobar PIPE file isn't exists, won't open\n");
	}
#endif
	
	int caps_was_pressed = 0;
	int caps_was_blocked = 0;
	int enter_was_pressed = 0;
	int enter_was_blocked = 0;
	
#ifdef DEBUG
	int lalt_was_pressed = 0;
	int ralt_was_pressed = 0;
#endif
	
	int capslock_was_activated = 0;
	
	char keys_return[KEYS_LIMIT];
	
	// reset previous press
	trigger_level3_release();
	detect_modes();
	
	pthread_t window_focus__thread;
	pthread_create(
		&window_focus__thread,
		NULL,
		window_focus__thread_handler,
		NULL
	);
	
	while (1) {
		
		int caps_is_pressed = 0;
		int enter_is_pressed = 0;
		int non_caps_is_pressed = 0;
		int non_enter_is_pressed = 0;
		
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
							non_caps_is_pressed = 1;
						}
						
						if (key_num == ENTER_KEY) {
							enter_is_pressed = 1;
						} else if (
							key_num != level3_key_code &&
							key_num != LCTRL_KEY &&
							key_num != RCTRL_KEY &&
							key_num != LSHIFT_KEY &&
							key_num != RSHIFT_KEY &&
							key_num != LALT_KEY &&
							key_num != RALT_KEY
						) {
							non_enter_is_pressed = 1;
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
		
		if (caps_was_blocked == 1 || non_caps_is_pressed == 1) {
			
			caps_was_pressed = 0;
			caps_was_blocked = 1;
			
			if (caps_is_pressed == 0 && non_caps_is_pressed == 0) {
				caps_was_blocked = 0;
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
			caps_was_blocked = 1;
			trigger_escape();
			reset_everything();
		}
		
		if (enter_was_blocked == 1 || non_enter_is_pressed == 1) {
			
			enter_was_pressed = 0;
			enter_was_blocked = 1;
			
			if (enter_is_pressed == 0 && non_enter_is_pressed == 0) {
				enter_was_blocked = 0;
			}
			
		} else if (enter_is_pressed == 1) {
#ifdef DEBUG
			if (enter_was_pressed == 0) {
				printf("DEBUG: Enter is pressed\n");
			}
#endif
			enter_was_pressed = 1;
		} else if (enter_was_pressed == 1) {
			enter_was_pressed = 0;
			enter_was_blocked = 1;
			trigger_enter();
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
			is_level3_on == 0 &&
			ralt_is_pressed == 0 &&
			caps_is_pressed == 1 &&
			lalt_is_pressed == 1
		) {
			trigger_level3_press();
		} else if (
			is_level3_on == 1 &&
			lalt_is_pressed == 0 &&
			ralt_is_pressed == 1
		) {
			reset_everything();
		}
		
		if (caps_is_pressed == 1 && enter_is_pressed == 1) {
			if (capslock_was_activated == 0) {
#ifdef DEBUG
				printf("DEBUG: Both Caps Lock and Enter is pressed\n");
#endif
				capslock_was_activated = 1;
				trigger_capslock();
			}
		} else {
			capslock_was_activated = 0;
		}
		
		detect_modes();
		usleep(IDLE_TIME); // idle
	}
	
	XCloseDisplay(dpy);
	return EXIT_SUCCESS;
}
