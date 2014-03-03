module MpdControl where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Lazy as M
import Data.Maybe
import Debug.Trace
import Network.MPD
import XMonad.Util.Run

pairForSong :: Song -> Maybe (String, Id)
pairForSong song = do
  songID <- sgId song
  let tags = sgTags song
  songTitles <- Title `M.lookup` tags 
  case songTitles of
    [Value titleBS] -> return (B.unpack titleBS, songID)

allSongsInPlaylist :: IO [(String, Id)]
allSongsInPlaylist = do
  response <- withMPD $ do
    songs <- playlistInfo Nothing
    return $ mapMaybe pairForSong songs
  case response of
    Left mpdError -> trace (show mpdError) (return [])
    Right x -> return x

pairsToDmenuString :: [(String, Id)] -> String
pairsToDmenuString = unlines . map fst

selectSong = do
  songs <- allSongsInPlaylist
  let input = pairsToDmenuString songs
  choice <- runProcessWithInput "dmenu" ["-i"] input
  unless (null $ choice) $ case (init choice) `lookup` songs of
    Just songID -> trace (show songID) $ (withMPD $ playId songID) >> return ()
    Nothing -> return ()