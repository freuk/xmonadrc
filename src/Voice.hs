{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Voice
  ( main,
    WMCommand (..),
    guake,
    pgBegin,
    pgEnd,
    pgUp,
    pgDown,
    closeTab,
    xDoKey,
  )
where

import Data.Char
import qualified Data.Map as Map
import Data.Universe
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Protolude hiding (Down)
import System.IO
import System.Process.Typed
import Prelude (String)

main :: IO ()
main = do
  getArgs >>= \case
    ("serve" : dir : threshold : _) -> do
      v <- asyncBound $ runVoice dir threshold
      void $ waitAnyCancel [v]
    ["dict"] -> printDict "gen" (Proxy @Dialect) <> printDict "gen" (Proxy @ReadCommand)
    _ -> putText "needs more input."

runVoice :: String -> String -> IO ()
runVoice dir th = listen (PocketSphinxCfg {vadThreshold = th, lmDirectory = dir, dicDirectory = dir})

-- | IO actions
sendVoice :: (MonadIO m) => WMCommand -> m ()
sendVoice = sendCommand "XMONAD_VOICE" . show

notifySend :: (MonadIO m, StringConv s String) => Int -> s -> m ()
notifySend timeout x =
  liftIO . runProcess_ $
    proc "notify-send" ["-t", show (timeout * 1000), toS x]

sendCommand :: (MonadIO m) => String -> String -> m ()
sendCommand addr s = liftIO $ do
  d <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d s False
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw a 32 m currentTime
    sendEvent d rw False structureNotifyMask e
    sync d False

xDoTool :: (MonadIO m) => [String] -> m ()
xDoTool x = runProcess_ $ proc "xdotool" (["sleep", "0.001"] <> x)

xDoKey :: (MonadIO m) => String -> m ()
xDoKey x = xDoTool ["key", x]

guake :: (MonadIO m) => m ()
guake = xDoKey "F11"

closeTab :: (MonadIO m) => m ()
closeTab = xDoKey "d"

pgUp :: (MonadIO m) => m ()
pgUp = xDoKey "Page_Up"

pgDown :: (MonadIO m) => m ()
pgDown = xDoKey "Page_Down"

pgBegin :: (MonadIO m) => m ()
pgBegin = xDoKey "KP_Begin"

pgEnd :: (MonadIO m) => m ()
pgEnd = xDoKey "KP_End"

data Weather = Weather Int Text

weather :: MonadIO m => m (Maybe Weather)
weather = liftIO $ do
  temperature <- readProcessStdout_ $ shell "cat ~/.cache/jelly-conky/weather.json | jq .main.temp"
  city <- readProcessStdout_ $ shell "cat ~/.cache/jelly-conky/weather.json | jq .name"
  return $ Weather <$> readMaybe (takeWhile (/= '.') (toS temperature)) <*> pure (toS city)

formatWeather :: Maybe Weather -> Text
formatWeather (Just (Weather t c)) = "It's " <> show t <> " degrees Celcius in " <> c <> "."
formatWeather Nothing = "Couldn't read weather from local cache."

speak :: (MonadIO m) => Text -> m ()
speak s = liftIO . runProcess_ $ proc "espeak" ["-a", "10", toS s]

data RootCommand
  = Wake
  | Sleep
  | GoDeaf
  | ListCommands
  | Temperature
  | StartReading
  deriving (Read, Show, Enum, Bounded, Universe)

data WMCommand
  = Telegram
  | Slack
  | Spotify
  | Firefox
  | FullScreen
  | Guake
  | PageUp
  | PageDown
  | PageBegin
  | PageEnd
  | CloseTab
  | Kill
  | Cycle
  | Sink
  | NewWorkspace
  | NextWorkspace
  | PreviousWorkspace
  | LockSession
  deriving (Read, Show, Enum, Bounded, Universe)

data ReadCommand
  = Up
  | Down
  | Exit
  deriving (Read, Show, Enum, Bounded, Universe)

class Voice a where
  sentence :: a -> String

instance (Voice a, Voice b) => Voice (Either a b) where
  sentence (Left x) = sentence x
  sentence (Right x) = sentence x

instance Voice ReadCommand where
  sentence = \case
    Up -> "previous"
    Down -> "go"
    Exit -> "exit"

instance Voice RootCommand where
  sentence = \case
    Wake -> "wakey wakey"
    Sleep -> "take a nap"
    GoDeaf -> "kill voice server"
    ListCommands -> "available"
    Temperature -> "gimme the temperature"
    StartReading -> "reader mode"

instance Voice WMCommand where
  sentence = \case
    Telegram -> "messages"
    Slack -> "slack"
    PageUp -> "page up"
    PageDown -> "page down"
    FullScreen -> "maximize"
    PageBegin -> "page begin"
    PageEnd -> "page bottom"
    CloseTab -> "close"
    Guake -> "terminal"
    Spotify -> "music"
    Firefox -> "chromium"
    Kill -> "kill"
    Cycle -> "cycle"
    Sink -> "sink"
    NewWorkspace -> "new workspace"
    NextWorkspace -> "switch"
    PreviousWorkspace -> "jump back"
    LockSession -> "lockdown"

fromVoice :: (Voice a, Universe a) => Text -> Maybe a
fromVoice = flip Map.lookup voiceMap
  where
    voiceMap :: (Universe a, Voice a) => Map Text a
    voiceMap = Map.fromList (universe <&> \c -> (toVoice c, c))

type a <^> b = Either a b

toVoice :: (Voice a) => a -> Text
toVoice = toS . dashify . sentence

dashify :: String -> String
dashify = fmap f
  where
    f :: Char -> Char
    f ' ' = '-'
    f x = toUpper x

printDict :: forall a. (Typeable a, Universe a, Voice a) => FilePath -> Proxy a -> IO ()
printDict dir p =
  writeWithExtension ".vocab" rawSentences
    <> writeWithExtension ".sent" surroundedSentences
  where
    rawSentences :: [Text]
    rawSentences = toVoice <$> universe @a
    surroundedSentences = rawSentences <&> ("<s> " <>) . (<> " </s>")
    writeWithExtension ext content =
      Protolude.writeFile
        (dir <> "/" <> dashify (show (typeRep p)) <> ext)
        (toS . mconcat $ intersperse "\n" content)

type Dialect = RootCommand <^> WMCommand

data ReadMode
  = Reading
  deriving (Eq)

data MainMode
  = Sleeping
  | Listening
  | Locked
  deriving (Eq)

data PocketSphinxCfg
  = PocketSphinxCfg
      { vadThreshold :: String,
        lmDirectory :: FilePath,
        dicDirectory :: FilePath
      }

doPocketSphinx ::
  (MonadIO m, Typeable a, Voice a, Universe a) =>
  Bool ->
  PocketSphinxCfg ->
  (Proxy a) ->
  m a
doPocketSphinx askWhat cfg prox@(Proxy :: Proxy a) = do
  ps <- startProcess pocketSphinx
  raw <- liftIO (hGetLine $ getStdout ps)
  stopProcess ps
  (fromVoice . toS $ raw :: Maybe a) & \case
    Nothing -> do
      when askWhat $
        notifySend 10 ("What? Try saying '" <> toS (sentence ListCommands) <> "'." :: Text)
      liftIO . putText $ "received nothing. Raw: " <> toS raw
      doPocketSphinx askWhat cfg prox
    Just x -> return x
  where
    modelName :: FilePath
    modelName = dashify . show $ typeRep prox
    basepath = dicDirectory cfg <> "/" <> modelName
    pocketSphinx :: ProcessConfig () Handle ()
    pocketSphinx =
      setStdout createPipe $
        proc
          "pocketsphinx_continuous"
          [ "-inmic",
            "yes",
            "-dict",
            basepath <> ".dic",
            "-lm",
            basepath <> ".lm",
            "-vad_threshold",
            vadThreshold cfg,
            "-logfn",
            "/dev/null"
          ]

listen :: PocketSphinxCfg -> IO ()
listen cfg = do
  speak "ready"
  evalStateT mainMode Sleeping
  where
    putIgnore :: WMCommand -> StateT MainMode IO ()
    putIgnore x = putText ("received : " <> show x <> " but ignored because locked.")
    lock :: StateT MainMode IO ()
    lock = speak "Locking!" >> sendVoice LockSession >> put Locked
    sleep :: StateT MainMode IO ()
    sleep = speak "I am going to sleep!" >> put Sleeping
    unlock :: StateT MainMode IO ()
    unlock = liftIO . runProcess_ $ "loginctl unlock-session"
    readMode :: (MonadIO m) => m ()
    readMode = doPocketSphinx True cfg (Proxy @ReadCommand) >>= \case
      Up -> pgUp >> readMode
      Down -> pgDown >> readMode
      Exit -> pass
    mainMode :: StateT MainMode IO ()
    mainMode = do
      command <- doPocketSphinx True cfg (Proxy @Dialect)
      get >>= \case
        Locked ->
          command & \case
            (Left Wake) -> speak "Unlocking." >> unlock >> put Listening
            (Left Sleep) -> sleep
            (Left GoDeaf) -> speak "Voice server killed." >> liftIO exitSuccess
            (Left _) -> pass
            (Right LockSession) -> putText "Already locked"
            (Right x) -> putIgnore x
        Sleeping ->
          command & \case
            (Left Wake) -> speak "At your service." >> unlock >> put Listening
            (Left Sleep) -> putText "Already sleeping"
            (Left GoDeaf) -> speak "Voice server killed." >> liftIO exitSuccess
            (Left _) -> pass
            (Right x) -> putIgnore x
        Listening -> do
          command & \case
            Left x -> notifySend 10 (sentence x <> " -> " <> show x)
            Right x -> notifySend 10 (sentence x <> " -> " <> show x)
          command & \case
            (Left Sleep) -> sleep
            (Left StartReading) -> readMode
            (Left Wake) -> unlock >> speak "I am already awake"
            (Left ListCommands) -> notifySend 50 (mconcat . intersperse "\n" $ sentence <$> (universe @Dialect))
            (Left GoDeaf) -> speak "Voice server killed." >> liftIO exitSuccess
            (Left Temperature) -> weather >>= speak . formatWeather
            (Right LockSession) -> lock
            (Right x) -> sendVoice x
      mainMode
