{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import MpdControl
import Network.MPD hiding ((=?))
import XMonad
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.EZConfig (additionalKeysP)
import System.IO

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

openOnBoot = [ "firefox"
             , "emacs"
             , "pavucontrol"
             ]

doOpenOnBoot = do
  psOutput <- T.pack <$> runProcessWithInput "ps" ["-e"] ""
  forM_ openOnBoot $ \progName ->
    unless (progName `T.isInfixOf` psOutput) $
      safeSpawnProg (T.unpack progName)

playPause = withMPD $ status >>=
            \st -> case stState st of
              Playing -> pause True
              _ -> play Nothing

allSongs = listAll (Path $ B.pack "")

recreatePlaylist = withMPD $ do
  runProcessWithInput "chmod" ["-R", "a+r", "/home/mitchell/Music"] []
  rescan []
  songs <- allSongs
  playlists <- listPlaylists
  when (name `elem` playlists) $ do
    load name
    clear
    rm name
  playlistAdd_ name (Path $ B.pack "")
  load name
  shuffle Nothing
  where name = (PlaylistName $ B.pack "Xmonad")

myManageHook = composeAll
    [ className =? "Steam" --> doFloat
    , className =? "Gimp" --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    , className =? "Firefox" --> doShift "2"
    , className =? "Emacs" --> doShift "3"
    , className =? "Pavucontrol" --> doShift "9"
    , className =? "Steam" --> doShift "5"
    ]

main = do
  xmproc <- spawnPipe "/home/mitchell/.cabal/bin/xmobar /home/mitchell/.xmobarrc"
  -- This is a hack to make the windows appear after xmonad starts up
  forkIO $ threadDelay 500000 >> doOpenOnBoot
  withMPD (pause True) -- Don't want music to automatically play on startup
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , terminal = "terminator"
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    -- , layoutHook = lessBorders OnlyFloat $ avoidStruts $ layoutHook defaultConfig
    , layoutHook = noBorders $ avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                {
                  ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 70
                }
    } `additionalKeysP` [
        ("<XF86AudioRaiseVolume>", raiseVolume 3 >> return ())
      , ("<XF86AudioLowerVolume>", lowerVolume 3 >> return ())
      , ("<XF86AudioMute>", toggleMute >> return ())
      , ("<XF86AudioPlay>", (liftIO playPause) >> return ())
      , ("<XF86AudioNext>", (liftIO $ withMPD next) >> return ())
      , ("<XF86AudioPrev>", (liftIO $ withMPD previous) >> return ())
      -- , ("<F6>", (liftIO $ withMPD previous) >> return ())
      -- , ("<F7>", (liftIO playPause) >> return ())
      -- , ("<F8>", (liftIO $ withMPD next) >> return ())
      , ("<F10>", liftIO selectQueueSong)
      , ("<F11>", liftIO selectPlaySong)
      , ("<F12>", liftIO (recreatePlaylist >> return ()))
      , ("M-m", sendMessage ToggleStruts)
      , ("M-f", safeSpawnProg "firefox")
      , ("M-s", unsafeSpawn "import -window root /home/mitchell/xwd-$(date +%s)$$.png")
      , ("M-e", safeSpawnProg "emacs")
      ]
