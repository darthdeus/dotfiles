import XMonad

main = xmonad defaultConfig
        { terminal    = "xfce4-terminal"
        , modMask     = mod4Mask
        , borderWidth = 3
        }
