{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import System.Posix.Unistd
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks    -- (3)  automatically avoid covering my
                                   --      status bar with windows
import XMonad.Hooks.ManageHelpers  -- (4)  for doCenterFloat, put floating

data Host = Desktop | Laptop deriving (Eq, Read, Show)

getHost :: IO Host
getHost = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
                  "vaio" -> Laptop
                  _      -> Desktop






spawnOn :: String -> String -> X ()
spawnOn workspace program = do
    spawn program
    windows $ W.greedyView workspace




myWorkspaces :: [WorkspaceId]
myWorkspaces = map show ([0 .. 9 :: Int])

main = do
    h <- getHost
    xmonad defaultConfig { borderWidth = 6
                         , manageHook  = manageSpawn <+> myManageHook
                         , layoutHook  = id
                             . smartBorders
                             . mkToggle (single NOBORDERS)
                             . mkToggle (single MIRROR)
                             . mkToggle (single FULL)
                             $ (layoutHook defaultConfig) ||| ThreeCol 1 (3/100) (1/4)
                         , modMask     = mod4Mask
                         , keys        = myKeys
                         , startupHook = sendMessage $ Toggle NOBORDERS
                         , workspaces  = myWorkspaces
                         , focusedBorderColor = "#0897eb"
                         , normalBorderColor = "#3a3a3a"
                         , terminal = "terminator"
                         }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn "terminator --profile=Floaty")

    --take a screenshot of entire display
    , ((modMask ,              xK_Print ), spawn "scrot ~/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S.png -d 1")

    --rotate between screen modes
    , ((modMask ,              xK_F7), spawn "screens")

    --take a screenshot of focused window
    , ((modMask .|. controlMask, xK_Print ), spawn "scrot ~/Pictures/Screenshots/window_%Y-%m-%d-%H-%M-%S.png -d 1 -u")

    -- launch gmrun
    , ((modMask,               xK_p     ), spawn "dmenu_run -i")

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask .|. shiftMask, xK_h     ), sendMessage Expand)


    , ((modMask,               xK_b     ), sendMessage $ Toggle NOBORDERS)
    , ((modMask,               xK_m     ), sendMessage $ Toggle MIRROR)
    , ((modMask,               xK_f     ), sendMessage $ Toggle FULL)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modMask,               xK_q     ), restart "xmonad" True)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [ ((m .|. modMask, k), windows $ f i)
         | (i, k) <- zip (myWorkspaces) ([xK_0 .. xK_9])
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    --
    -- my Additional Keybindings
    --
    [ ((modMask .|. shiftMask,   xK_b           ), spawn "chromium")
    , ((modMask,                 xK_l           ), spawn "gnome-screensaver-command -l")
    , ((0                     , 0x1008FF10), spawn "amixer set Master 2-")
    , ((0                     , 0x1008FF11), spawn "amixer set Master 2+")
    , ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
    ]



myManageHook :: ManageHook
myManageHook = composeAll [ appName =? "trello.com"                    --> doShift "0"
                          , appName =? "play.google.com__music_listen" --> doShift "2"
                          , appName =? "app.couple.me__login"          --> doShift "3"
                          , appName =? "www.twitter.com"               --> doShift "3"
                          , appName =? "bgv8.slack.com"                --> doShift "3"
                          , appName =? "www.evernote.com__Home.action" --> doShift "9"
                          , appName =? "gmail.com"                     --> doShift "9"
                          , appName =? "www.crunchyroll.com"           --> doFloat
                          ]
