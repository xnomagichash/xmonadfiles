import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
--import XMonad.Hooks.EwmhDesktops
--import XMonad.Config.Gnome

startup :: X ()
startup = do
    --spawnOn "6" "play"
    --spawnOn "8" "twitter.com"
    --spawnOn "6" "couple"
    spawnOn "1" "terminator --profile=Floaty"
    --spawnOn "0" "trello"
    --spawnOn "8" "evernote"

spawnOn :: String -> String -> X ()
spawnOn workspace program = do
    spawn program
    windows $ W.greedyView workspace

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show ([0 .. 9 :: Int])

main = xmonad gnomeConfig
     { borderWidth = 0
     , layoutHook  = smartBorders (layoutHook gnomeConfig) ||| ThreeCol 1 (3/100) (1/4)
     , startupHook = startup
     , modMask     = mod4Mask
     , keys        = myKeys
     , workspaces  = myWorkspaces
     , focusedBorderColor = "#000000"
     , normalBorderColor = "#000000"
     --, handleEventHook    = fullscreenEventHook
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
    [ ((modMask,                 xK_b           ), spawn "chromium")
    , ((modMask,                 xK_l           ), spawn "gnome-screensaver-command -l")
    , ((0                     , 0x1008FF10), spawn "amixer set Master 2-")
    , ((0                     , 0x1008FF11), spawn "amixer set Master 2+")
    , ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
    ]
