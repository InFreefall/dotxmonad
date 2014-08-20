import Control.Monad
import qualified Data.ByteString.Char8 as B
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
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

main = do
  xmproc <- spawnPipe "/home/mitchell/.cabal/bin/xmobar /home/mitchell/.xmobarrc"
  withMPD (pause True) -- Don't want music to automatically play on startup
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , terminal = "terminator"
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = lessBorders OnlyFloat $ avoidStruts $ layoutHook defaultConfig
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
      ]
