#!/usr/bin/env bash
set -ex

cd /tmp
git clone https://aur.archlinux.org/yay-bin.git
cd yay-bin
makepkg -sri
