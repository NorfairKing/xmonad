{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module MyXmonad
  ( myXmonad
  ) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Options.Applicative
import Prelude hiding (mod)
import System.IO (Handle)
import XMonad
import qualified XMonad.Actions.PhysicalScreens as S
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Run (hPutStrLn, spawnPipe)

myXmonad :: IO ()
myXmonad = do
  Settings {..} <- execParser parseSettings
  xmprocs <- spawnXmobars setXmobar
  launch $
    def
      { normalBorderColor = "#657b83"
      , focusedBorderColor = "#268BD2"
      , terminal = "urxvt"
      , layoutHook = myLayoutHook
      , manageHook = myManageHook
      , handleEventHook = fullscreenEventHook
      , workspaces = myWorkspaces setKeyboard
      , modMask = mod4Mask
      , keys = myKeys setKeyboard
      , mouseBindings = myMouse
      , borderWidth = 1
      , logHook = myLogHook xmprocs
      , startupHook = myStartupHook
      }

data Settings =
  Settings
    { setXmobar :: FilePath
    , setKeyboard :: KeyBoard
    }
  deriving (Show, Eq)

parseSettings :: ParserInfo Settings
parseSettings = info (helper <*> parseArgs) inf
  where
    inf = fullDesc <> progDesc description
    description = "Xmonad"

parseArgs :: Parser Settings
parseArgs =
  Settings <$>
  strOption
    (mconcat
       [ long "xmobar"
       , metavar "PATH"
       , help "The path to the xmobar binary"
       , value "xmobar"
       , showDefault
       ]) <*>
  option
    (maybeReader parseKeyboard)
    (mconcat
       [ long "keyboard"
       , metavar "KEYBOARD"
       , help $
         "Which keyboard settings to use, options: " <>
         show (map renderKeyboard [minBound .. maxBound])
       ])

data KeyBoard
  = KinesisDvorak
  | LaptopDvorak
  deriving (Show, Eq, Enum, Bounded)

parseKeyboard :: String -> Maybe KeyBoard
parseKeyboard =
  \case
    "kinesis.dvorak" -> Just KinesisDvorak
    "laptop.dvorak" -> Just LaptopDvorak
    _ -> Nothing

renderKeyboard :: KeyBoard -> String
renderKeyboard =
  \case
    KinesisDvorak -> "kinesis.dvorak"
    LaptopDvorak -> "laptop.dvorak"

spawnXmobars ::FilePath -> IO [Handle]
spawnXmobars xmobar = do
  displays <- countScreens
  forM [0 :: Int .. displays - 1] $ \d -> spawnPipe $ unwords [xmobar, "--screen", show d]

myManageHook :: ManageHook
myManageHook = manageDocks

myLogHook :: [Handle] -> X ()
myLogHook xmprocs =
  dynamicLogWithPP $
  xmobarPP
    { ppOutput = \s -> mapM_ (`hPutStrLn` s) xmprocs
    , ppTitle = xmobarColor "#268BD2" "" . shorten 80
    , ppCurrent = xmobarColor "#268BD2" "" . wrap "[" "]"
    , ppVisible = xmobarColor "#657b83" "" . wrap "(" ")"
    , ppUrgent = xmobarColor "#0000ff" "" . wrap "{" "}"
    }

myWorkspaces :: KeyBoard -> [WorkspaceId]
myWorkspaces =
  \case
    KinesisDvorak -> kinesisDvorakWorkspaces
    LaptopDvorak -> laptopDvorakWorkspaces

kinesisDvorakWorkspaces :: [WorkspaceId]
kinesisDvorakWorkspaces = keyboardMappingWorkspaces kinesisDvorakKeyboardMapping

laptopDvorakWorkspaces :: [WorkspaceId]
laptopDvorakWorkspaces = keyboardMappingWorkspaces laptopDvorakKeyboardMapping

type KeyboardKeyMapping = [(Char, KeySym)]

keyboardMappingWorkspaces :: KeyboardKeyMapping -> [WorkspaceId]
keyboardMappingWorkspaces = map $ pure . fst

kinesisDvorakKeyboardMapping :: KeyboardKeyMapping
kinesisDvorakKeyboardMapping =
  [ ('p', xK_p)
  , ('g', xK_g)
  , ('c', xK_c)
  , ('r', xK_r)
  , ('l', xK_l)
  , ('a', xK_a)
  , ('o', xK_o)
  , ('e', xK_e)
  , ('u', xK_u)
  , ('i', xK_i)
  , ('d', xK_d)
  , ('h', xK_h)
  , ('t', xK_t)
  , ('n', xK_n)
  , ('s', xK_s)
  , ('q', xK_q)
  , ('j', xK_j)
  , ('k', xK_k)
  , ('x', xK_x)
  , ('b', xK_b)
  , ('m', xK_m)
  , ('w', xK_w)
  , ('v', xK_v)
  , ('z', xK_z)
  ]

laptopDvorakKeyboardMapping :: KeyboardKeyMapping
laptopDvorakKeyboardMapping =
  [ ('g', xK_g)
  , ('c', xK_c)
  , ('r', xK_r)
  , ('h', xK_h)
  , ('t', xK_t)
  , ('n', xK_n)
  , ('m', xK_m)
  , ('v', xK_v)
  , ('w', xK_w)
  ]

myKeys :: KeyBoard -> XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys =
  \case
    KinesisDvorak -> kinesisDvorakKeys
    LaptopDvorak -> laptopDvorakKeys

kinesisDvorakKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
kinesisDvorakKeys XConfig {modMask = mod, terminal} =
  M.fromList
    [ ((mod, xK_f), spawn terminal)
    , ((mod, xK_y), closeWindow)
        -- Top row layout: [{+(= *)!}]
    , ((mod, xK_braceleft), focusPrevScreen)
    , ((mod .|. shiftMask, xK_braceleft), shiftPrevScreen)
    , ((mod, xK_parenleft), lessWindows)
    , ((mod, xK_plus), focusWindowDown)
    , ((mod .|. shiftMask, xK_plus), swapWindowDown)
    , ((mod, xK_equal), shrinkWindow)
    , ((mod, xK_asterisk), expandWindow)
    , ((mod, xK_exclam), focusWindowUp)
    , ((mod, xK_parenright), moreWindows)
    , ((mod .|. shiftMask, xK_exclam), swapWindowUp)
    , ((mod, xK_braceright), focusNextScreen)
    , ((mod .|. shiftMask, xK_braceright), shiftNextScreen)
    , ((mod, xK_space), nextLayout)
    , ((mod, xK_Tab), nextWindow)
    , ((mod .|. shiftMask, xK_Tab), previousWindow)
    , ((mod, xK_period), internet)
    , ((mod, xK_BackSpace), tileAgain)
    ] `M.union`
  keyboardMappingNavigationKeys mod kinesisDvorakKeyboardMapping

laptopDvorakKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
laptopDvorakKeys XConfig {modMask = mod, terminal} =
  M.fromList
    [ ((mod, xK_f), spawn terminal)
    , ((mod, xK_d), closeWindow)
    , ((mod, xK_o), focusWindowDown)
    , ((mod .|. shiftMask, xK_o), swapWindowDown)
    , ((mod, xK_e), focusWindowUp)
    , ((mod .|. shiftMask, xK_e), swapWindowUp)
    , ((mod, xK_comma), shrinkWindow)
    , ((mod, xK_period), expandWindow)
    , ((mod, xK_q), lessWindows)
    , ((mod, xK_j), moreWindows)
    , ((mod, xK_space), nextLayout)
    , ((mod, xK_Tab), nextWindow)
    , ((mod .|. shiftMask, xK_Tab), previousWindow)
    , ((mod, xK_b), internet)
    , ((mod, xK_BackSpace), tileAgain)
    ] `M.union`
  keyboardMappingNavigationKeys mod laptopDvorakKeyboardMapping

keyboardMappingNavigationKeys :: ButtonMask -> KeyboardKeyMapping -> Map (ButtonMask, KeySym) (X ())
keyboardMappingNavigationKeys mod km =
  M.fromList
    [ ((mask .|. mod, key), windows $ switch [ws])
    | (ws, key) <- km
    , (switch, mask) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

internet :: X ()
internet = spawn "firefox"

-- Select the next layout.
nextLayout :: X ()
nextLayout = sendMessage NextLayout

-- Select the next window.
nextWindow :: X ()
nextWindow = windows W.focusDown

-- Select the previous window.
previousWindow :: X ()
previousWindow = windows W.focusUp

-- Close the selected window
closeWindow :: X ()
closeWindow = kill

-- Shrink the master window.
shrinkWindow :: X ()
shrinkWindow = sendMessage Shrink

-- Expand the master window.
expandWindow :: X ()
expandWindow = sendMessage Expand

focusWindowUp :: X ()
focusWindowUp = windows W.focusUp

swapWindowUp :: X ()
swapWindowUp = windows W.swapUp

focusNextScreen :: X ()
focusNextScreen = S.onNextNeighbour def W.view

focusPrevScreen :: X ()
focusPrevScreen = S.onPrevNeighbour def W.view

shiftNextScreen :: X ()
shiftNextScreen = S.onNextNeighbour def W.shift

shiftPrevScreen :: X ()
shiftPrevScreen = S.onPrevNeighbour def W.shift

-- Select the previous window.
focusWindowDown :: X ()
focusWindowDown = windows W.focusDown

-- Swap the selected window with the previous window.
swapWindowDown :: X ()
swapWindowDown = windows W.swapDown

-- Push selected window back into tiling
tileAgain :: X ()
tileAgain = withFocused $ windows . W.sink

-- Increment the number of windows in the master area.
moreWindows :: X ()
moreWindows = sendMessage (IncMasterN 1)

-- Decrement the number of windows in the master area.
lessWindows :: X ()
lessWindows = sendMessage (IncMasterN (-1))

myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse XConfig {modMask = mod} =
  M.fromList
        -- Left_mouse_button    Set the window to floating mode and mov by dragging
    [ ((mod, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
        -- Right_mouse_button   Set the window to floating mode and resize by dragging
    , ((mod, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

myLayoutHook = avoidStruts (full ||| tiled ||| mtiled)
  where
    -- Fullscreen (default)
    full = named "full" $ mySpacing $ noBorders Full
    -- Split vertically with phi as the ratio between the widths
    tiled = named "tiled" $ mySpacing $ Tall 1 (5 / 100) (1 / (toRational phi))
    -- Split horizonatlly in the same way
    mtiled = named "mtiled" $ Mirror tiled
    phi = (1 + sqrt 5) / 2.0 :: Double
    mySpacing = spacingRaw False myBorder False myBorder True
    myBorder = Border tileSpacing tileSpacing tileSpacing tileSpacing
    tileSpacing = 3

-- | Startup
myStartupHook :: X ()
myStartupHook
    -- Make Java GUI's work
 = setWMName "LG3D"
