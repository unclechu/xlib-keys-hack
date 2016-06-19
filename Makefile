NAME ?= xlib-escape-key-hack
BUILD_DIR ?= ./build/

$(NAME): clean
	mkdir "$(BUILD_DIR)"
	gcc \
		./src/main.c \
		-lX11 -lXtst \
		-std=c99 \
		-o "$(BUILD_DIR)/$(NAME)"

clean:
	rm -rf "$(BUILD_DIR)"

all: $(NAME)
