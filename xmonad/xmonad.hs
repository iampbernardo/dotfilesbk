import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Config.Xfce


main = xmonad xfceConfig
    { terminal    = "terminator"
     ,normalBorderColor = "#9733AB"  
     ,focusedBorderColor = "#456def"
     , modMask     = mod4Mask  -- Rebinds Mod to windows key
    }
