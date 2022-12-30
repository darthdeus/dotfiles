# Dotfiles

My config files for bash, git, ruby and other useful scripts.

### Company completion bug:

:foo, M-x company-complete

- none: no, yes
- helm: yes, yes
- ido: no, yes
- ivy: no, yes
- vertico: yes, yes


## Rust-Analyzer LSP config

    local ra_opts = {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = {
        ["rust-analyzer"] = {
          checkOnSave = {
          },
          cargo = {
            extraEnv = { IN_RUST_ANALYZER = "1" },
          },
          rustfmt = {
          }
        }
      }
    }


## TODO

- rofi-emoji, rofi-pass
- [ ] try https://github.com/sharkdp/bat
- [ ] zplug/antigen/other/none?
- [x] ag build in tmp with copy to $HOME
- [ ] working vim/nvim build in $HOME
- [ ] create a working and up-to-date install script
- [ ] fix `tmux/zoom.sh`
- [ ] fix `tmux/copy.sh`

## Things to try

- terminal tools https://terminalsare.sexy/#universal

- pip-app https://github.com/sharat87/pip-app

- zsh-pipenv https://github.com/iboyperson/zsh-pipenv/blob/master/\_pipenv

- howdoi https://github.com/gleitz/howdoi
- check out newer versions of htop http://hisham.hm/htop/
- glances https://github.com/nicolargo/glances
- rtv https://github.com/michael-lazar/rtv
- googler https://github.com/jarun/googler
- nix https://ariya.io/2016/05/nix-as-os-x-package-manager
- antigen https://github.com/zsh-users/antigen
- Check out awesome-zsh https://github.com/unixorn/awesome-zsh-plugins#generic-zsh<Paste>

- pycm https://github.com/sepandhaghighi/pycm
