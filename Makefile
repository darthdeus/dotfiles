default: all

all: install-dwm install-st symlink

install-dwm:
	cd dwm; make; ln -nsf $(HOME)/.dotfiles/dwm/dwm $(HOME)/.dotfiles/bin/dwm

install-st:
	cd st; make; ln -nsf $(HOME)/.dotfiles/st/st $(HOME)/.dotfiles/bin/st

symlink:
	./link.sh
