default: all

all: install-dwm install-st symlink malloc_dump

install-dwm:
	cd dwm; make; ln -nsf $(HOME)/.dotfiles/dwm/dwm $(HOME)/.dotfiles/bin/dwm

install-st:
	cd st; make; ln -nsf $(HOME)/.dotfiles/st/st $(HOME)/.dotfiles/bin/st

symlink:
	./link.sh

malloc_dump:
	gcc -shared -o malloc_dump.so malloc_dump.c -fPIC

base16-shell:
	mkdir -p $(HOME)/.config
	git clone https://github.com/chriskempson/base16-shell $(HOME)/.config/base16-shell

install-conda:
	wget -c https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh
	bash /tmp/miniconda.sh -b -p $(HOME)/.miniconda -s
	rm /tmp/miniconda.sh

pyenv:
	git clone https://github.com/pyenv/pyenv.git ~/.pyenv
