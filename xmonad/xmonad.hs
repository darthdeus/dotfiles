import XMonad

main = xmonad defaultConfig
        { terminal    = "~/bin/st -e tmux"
        , modMask     = mod4Mask
        , borderWidth = 3
        }
