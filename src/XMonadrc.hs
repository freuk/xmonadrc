module XMonadrc
  ( main,
    xDoKey,
  )
where

import qualified Data.Map as M
import Graphics.X11.Xlib (Window)
import Protolude
import Voice hiding (main)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.Minimize
import XMonad.Actions.MouseResize
import XMonad.Actions.Search
import XMonad.Actions.WindowGo
  ( raiseMaybe,
    raiseMaybe,
    runOrRaise,
  )
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Pass
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
import qualified XMonad.StackSet as W
import XMonad.Util.NoTaskbar
import XMonad.Util.Paste (pasteSelection)
import XMonad.Util.Run
  ( runInTerm,
  )
import XMonad.Util.Scratchpad
import qualified Prelude

myPlacement :: Placement
myPlacement = withGaps (16, 0, 16, 0) (smart (0.5, 0.5))

main :: IO ()
main =
  xmonad $ docks $
    ewmh
      def
        { modMask = mod4Mask,
        manageHook =
            manageHook def
              <+> composeAll
                [ className =? "kittyFromXmonad" --> unfloat,
                  className =? "Firefox" --> unfloat,
                  className =? "qutebrowser" --> unfloat,
                  className =? "scratchpad" --> placeHook myPlacement,
                  className =? "scratchpad" --> noTaskbar,
                  className =? "kittybg" --> doIgnore,
                  className =? "pavucontrol" --> doRectFloat (W.RationalRect 0.4 0.4 0.2 0.2),
                  className =? "grc-prompter" --> doRectFloat (W.RationalRect 0.45 0.45 0.1 0.1),
                  isFullscreen --> doFullFloat,
                  pure True --> doRectFloat (W.RationalRect 0.2 0.2 0.6 0.6)
                ]
              <+> doCenterFloat
              <+> manageScratchPad
              <+> manageDocks,
          layoutHook = mkToggle (single NBFULL) $ avoidStruts myLayout,
          handleEventHook =
            handleEventHook def
              <+> minimizeEventHook
              <+> fullscreenEventHook
              <+> docksEventHook
              <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
              <+> serverModeEventHookF "XMONAD_VOICE" (voice . readMaybe),
          startupHook = setWMName "main",
          keys = myKeys,
          workspaces = myWorkspaces,
          borderWidth = 2,
          mouseBindings = myMouseBindings,
          normalBorderColor = "#666666",
          focusedBorderColor = "#FF0000",
          terminal = "kitty"
        }
  where
    unfloat :: XMonad.Query (Endo (W.StackSet i l Window s sd))
    unfloat = ask >>= doF . W.sink

voice :: Maybe WMCommand -> X ()
voice Nothing = putText "decoding voice server command to Nothing"
voice (Just x) =
  x & \case
    FullScreen -> xDoKey "F5"
    Guake -> guake
    CloseTab -> closeTab
    PageDown -> pgDown
    PageUp -> pgUp
    PageBegin -> pgBegin
    PageEnd -> pgEnd
    Firefox -> runOrRaise "chromium" (className =? "chromium-browser")
    Telegram -> runOrRaise "telegram-desktop" (className =? "TelegramDesktop")
    Slack -> runOrRaise "slack" (className =? "Slack")
    Spotify -> runOrRaise "spotify" (className =? "Spotify")
    NewWorkspace -> viewEmptyWorkspace
    NextWorkspace -> moveTo Next NonEmptyWS
    PreviousWorkspace -> moveTo Prev NonEmptyWS
    Kill -> kill
    Cycle -> windows W.focusDown
    Sink -> withFocused $ windows . W.sink
    LockSession -> spawn "loginctl lock-session"

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect 0.1 0.1 0.8 0.1)

myLayout =
  spacingRaw False (Border 3 3 3 3) True (Border 3 3 3 3) True
    . minimize
    . mouseResize
    $ emptyBSP

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Integer]

myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:PragmataPro Mono Liga:size=12",
      bgColor = "#000000",
      fgColor = "#DDDDDD",
      fgHLight = "#FFFFFF",
      bgHLight = "#333333",
      borderColor = "#666666",
      promptBorderWidth = 2,
      position = CenteredAt (1 % 150) (3 % 7),
      height = 23,
      searchPredicate = fuzzyMatch,
      historySize = 256,
      defaultText = "",
      historyFilter = identity,
      showCompletionOnTab = False,
      autoComplete = Just 1
    }

myNoAutoCompleteXPConfig :: XPConfig
myNoAutoCompleteXPConfig = myXPConfig {autoComplete = Nothing}

myKeys :: XConfig l -> Map (KeyMask, KeySym) (X ())
myKeys XConfig {XMonad.modMask = modMask} =
  M.fromList
    [ ((modMask, xK_Return), spawn "kitty --class=kittyFromXmonad"),
      ((modMask .|. shiftMask, xK_Return), spawn "kitty"),
      ((modMask, xK_p), mateRun),
      ((modMask .|. shiftMask, xK_h), windows W.swapUp),
      ((modMask .|. shiftMask, xK_l), windows W.swapDown),
      ((modMask, xK_h), windows W.focusUp),
      ((modMask, xK_l), windows W.focusDown),
      ((modMask, xK_v), pasteSelection),
      ((modMask, xK_a), raiseMaybe (runInTerm "--title alot" "bash -c alot") (title =? "alot")),
      ((modMask, xK_o), raiseMaybe (runInTerm "--title ikhal" "ikhal") (title =? "ikhal")),
      ((modMask, xK_f), runOrRaise "chromium" (className =? "chromium-browser")),
      ((modMask, xK_n), viewEmptyWorkspace),
      ((modMask, xK_u), sendMessage $ Toggle NBFULL),
      ((modMask, xK_e), runOrRaisePrompt myXPConfig),
      ((modMask, xK_BackSpace), spawn "rofi-pow"),
      ((modMask, xK_Delete), spawn "slock"),
      ((modMask, xK_k), raiseMaybe (runInTerm "--title khal -o background_opacity=0.7" "zikhal") (title =? "ikhal")),
      ((modMask .|. shiftMask, xK_c), passGeneratePrompt myXPConfig),
      ((modMask, xK_w), windowPrompt myXPConfig Goto wsWindows),
      ((modMask, xK_s), promptSearch myNoAutoCompleteXPConfig myEngine),
      ((modMask, xK_g), promptSearch myNoAutoCompleteXPConfig google),
      ((modMask, xK_y), spawn "BROWSER=mpv kitty -o background_opacity=0.7 googler -w youtube.com"),
      ((modMask, xK_z), spawn "rofi-sea"),
      ((modMask, xK_r), spawn "autorandr rotate"),
      ((modMask .|. shiftMask, xK_r), spawn "autorandr home"),
      ((modMask, xK_F8), spawn "xbacklight -dec 10"),
      ((modMask, xK_F9), spawn "xbacklight -inc 10"),
      ((modMask .|. shiftMask, xK_apostrophe), kill),
      ((modMask, xK_m), withFocused minimizeWindow),
      ((modMask, xK_space), spawn "guake-toggle"),
      ((modMask .|. shiftMask, xK_space), withFocused $ windows . W.sink),
      ((modMask, xK_Tab), moveTo Next NonEmptyWS),
      ((modMask .|. shiftMask, xK_Tab), moveTo Prev NonEmptyWS)
    ]

myMouseBindings :: XConfig l -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modMask} =
  M.fromList
    [ ( (modMask, button1),
        \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
      ),
      ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow),
      ( (modMask, button3),
        \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
      )
    ]

myEngine :: SearchEngine
myEngine = searchEngineF "mymulti" searchFunc
  where
    searchFunc s@(formatS -> formatted)
      | "w " `isPrefixOf` s = "http://www.wordreference.com/fren/" <> formatted
      | "ho " `isPrefixOf` s =
        "https://hoogle.haskell.org/?hoogle="
          <> formatted
      | "ha " `isPrefixOf` s =
        "https://hackage.haskell.org/packages/search?terms="
          <> formatted
      | "http" `isPrefixOf` s = s
      | "www" `isPrefixOf` s = s
      | otherwise = "https://google.com/search?q=" <> formatted
    formatS s = escape (Prelude.tail $ dropWhile (/= ' ') s)

mateRun :: X ()
mateRun =
  withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"
    io $ allocaXEvent $ \e -> do
      setEventType e clientMessage
      setClientMessageEvent e rw mate_panel 32 panel_run 0
      sendEvent dpy rw False structureNotifyMask e
      sync dpy False
