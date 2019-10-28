a:
	xrdb ~/.Xresources
	urxvt

default: link

all: install-dwm install-st symlink malloc_dump

submodules:
	git submodule update --init --recursive

install-dwm:
	cd third-party/dwm; make; ln -nsf $(HOME)/.dotfiles/third-party/dwm/dwm $(HOME)/.dotfiles/bin/dwm

install-st:
	cd third-party/st; make; ln -nsf $(HOME)/.dotfiles/third-party/st/st $(HOME)/.dotfiles/bin/st

link:
	./utils/link.sh

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

pyenv-virtualenv:
	git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv

neovim-deps:
	./utils/neovim-deps.sh

install-kitty:
	curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin launch=n
