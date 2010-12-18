#!/bin/bash
echo "Installing required packages"
./script/install/required.sh
echo "Linking dotfiles"
./script/install/link.sh
echo "Installing vcprompt"
./script/install/vcprompt.sh
echo "Installing rvm"
./script/install/rvm.sh
