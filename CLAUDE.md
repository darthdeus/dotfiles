# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a comprehensive dotfiles repository containing configuration for development environments across Linux systems. The primary focus is on Neovim and Zsh, with additional configurations for various terminal tools and window managers.

## Key Components

- **nvim/**: Modern Neovim configuration using Lazy.nvim plugin manager with LSP, completion, and extensive plugins
- **zsh/**: Comprehensive Zsh configuration with antidote plugin manager, custom prompt, and numerous utility functions
- **bin/**: Collection of custom utility scripts and tools
- **third-party/**: Git submodules for external tools (antidote, base16-shell, dwm, st, tpm, etc.)
- **deps/**: Package dependency lists for different Linux distributions
- **config/**: Application-specific configurations (weechat, etc.)

## Common Development Tasks

### Setup and Installation
- `make link` - Symlink dotfiles to appropriate locations in $HOME
- `make submodules` - Initialize and update git submodules
- `make all` - Full installation including dwm/st builds and symlinks
- `./utils/link.sh` - Core linking script for dotfiles

### Neovim Management
- Neovim uses Lazy.nvim plugin manager
- Configuration is in `nvim/init.lua` with modular Lua files in `nvim/lua/user/`
- LSP setup supports Rust, Python, Lua, JSON, Bash, and more via Mason
- Uses rust-analyzer with custom settings for Rust development
- Copilot integration is configurable via `_G.copilot_enabled`

### Zsh Configuration
- Main config: `zsh/zshrc`
- Plugin management via antidote with plugins listed in `zsh/plugins.txt`
- Custom functions in `zsh/funcs/` directory
- Modular library files in `zsh/lib/` for different functionality areas
- Custom prompt with git integration in `zsh/prompt.zsh`

### Package Dependencies
- Install system packages using files in `deps/` directory
- `deps/arch-deps` for Arch Linux packages
- `deps/ubuntu-deps` for Ubuntu packages
- `deps/cargo-deps` for Rust cargo packages

## Architecture Notes

### Plugin Management
- Neovim: Lazy.nvim with extensive plugin ecosystem
- Zsh: antidote for fast zsh plugin loading
- Tmux: tpm (tmux plugin manager) via third-party submodule

### LSP and Completion
- Uses nvim-lspconfig with Mason for LSP server management
- Completion via nvim-cmp with multiple sources (LSP, buffer, path, etc.)
- Rust development optimized with rustacean.nvim and custom rust-analyzer settings

### Custom Utilities
- Various scripts in `bin/` for development workflow
- `bin/v` - main vim/nvim launcher script
- Custom functions in `zsh/funcs/` for shell utilities
- Window manager configurations for dwm and i3

### File Organization
- Dotfiles use symlinks from ~/.dotfiles to appropriate $HOME locations
- Modular configuration split into logical components
- Third-party tools managed as git submodules for version control
