import XMonad

main = xmonad defaultConfig
        { terminal    = "st -e tmux"
        , modMask     = mod4Mask
        , borderWidth = 3
        }
