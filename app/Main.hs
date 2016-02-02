module Main where

import Lib
import System.Environment (getArgs)
import Data.ByteString as BS (readFile)
import Torrent.Metainfo

main :: IO ()
main = do
  fname <- head <$> getArgs
  file <- BS.readFile fname
  print $ parseBencode file >>= parseMetainfo
