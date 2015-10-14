import XMonad

-- TODO - automatically detect which version of st should be run
main = xmonad defaultConfig
        { terminal    = "~/.dotfiles/bin/st -e tmux"
        , modMask     = mod4Mask
        , borderWidth = 3
        }
