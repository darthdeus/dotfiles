default: all

all: install-dwm install-st symlink

install-dwm:
	cd dwm; make; ln -nsf $(HOME)/.dotfiles/dwm/dwm $(HOME)/.dotfiles/bin/dwm

install-st:
	cd st; make; ln -nsf $(HOME)/.dotfiles/st/st $(HOME)/.dotfiles/bin/st

symlink:
	./link.sh

base16-shell:
	mkdir -p $(HOME)/.config
	git clone https://github.com/chriskempson/base16-shell $(HOME)/.config/base16-shell
