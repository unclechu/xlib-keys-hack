NAME ?= xlib-keys-hack
BUILD_DIR ?= ./build/
DEBUG ?= 0

ifeq ($(DEBUG),1)
	DEBUG_FLAG = -D DEBUG
else
	DEBUG_FLAG =
endif

$(NAME): clean
	mkdir "$(BUILD_DIR)"
	gcc \
		./src/main.c \
		-lX11 -lXtst -lpthread \
		-std=c11 \
		-Ofast \
		-D _XOPEN_SOURCE=600 \
		$(DEBUG_FLAG) \
		-o "$(BUILD_DIR)/$(NAME)"

clean:
	rm -rf "$(BUILD_DIR)"

all: $(NAME)
