module Lib where

  import Data.ByteString
  import Data.Attoparsec

  import Bencode
  import Torrent.Metainfo
  import Torrent.PeerWireProtocol


  parseBencode :: ByteString -> Maybe Bencode
  parseBencode = maybeResult . parse bencode
