default: all

all: install-dwm install-st symlink

install-dwm:
	cd dwm; make; cp dwm ../bin

install-st:
	cd st; make; cp st ../bin

symlink:
	./link.sh
