module Lib where

  import Data.ByteString
  import Data.Attoparsec

  import Bencode
  import Torrent.Metainfo


  parseBencode :: ByteString -> Maybe Bencode
  parseBencode = maybeResult . parse bencode
