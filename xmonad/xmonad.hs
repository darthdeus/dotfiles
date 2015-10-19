import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare

-- main :: IO ()
-- main = do
--   conf <- statusBar "dzen2 -p -xs 1 -ta l"
--     dzenPP
--       { ppHidden = pad
--       , ppTitle = pad . dzenColor "#bbb" "" . dzenEscape
--       , ppLayout = const ""
--       , ppSort = getSortByXineramaRule
--       }
--     toggleStrutsKey
--     $ withUrgencyHook NoUrgencyHook
--     $ defaultConfig
--         { terminal    = "~/.dotfiles/bin/st -e tmux"
--         , modMask     = mod4Mask
--         , borderWidth = 2
--         }
--
--   xmonad conf
--
-- toggleStrutsKey XConfig { modMask = modm } = (modm, xK_b)

-- TODO - automatically detect which version of st should be run
main = xmonad defaultConfig
        { terminal    = "~/.dotfiles/bin/st"
        , modMask     = mod4Mask
        , borderWidth = 2
        }
