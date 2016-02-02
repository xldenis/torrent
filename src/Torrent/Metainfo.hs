{-# LANGUAGE OverloadedStrings #-}
module Torrent.Metainfo where

import Control.Monad (join)
import Control.Lens
import Data.List.Split
import Data.ByteString as BS

import Bencode

data Torrent = Metainfo {announce :: ByteString, info :: Info}
  deriving (Show, Eq)

data Info = Info {name :: ByteString, pieceLength :: Integer, pieces :: [SHA], fileList :: [File], hash :: SHA}
  deriving (Show, Eq)

type SHA = ByteString

data File = File {length :: Integer, path :: [ByteString]}
  deriving (Show, Eq)


parseMetainfo :: Bencode -> Maybe Torrent
parseMetainfo bv = Metainfo
               <$> (bv ^? bkey "announce" . bstring)
               <*> (bv ^? bkey "info" >>= parseInfoDict)

parseInfoDict :: Bencode -> Maybe Info
parseInfoDict bv = Info
                 <$> bv ^? bkey "name" . bstring
                 <*> bv ^? bkey "piece length" . binteger
                 <*> ((chunk 20) <$> (bv ^? bkey "pieces" . bstring))
                 <*> (join $ files <$> bv ^? bkey "files" . blist)
                 <*> (return "hash")
  where chunk n "" = []
        chunk n bs = let (h, t) = BS.splitAt n bs in h : (chunk n t)


files :: [Bencode] -> Maybe [File]
files b = mapMOf each toFile b
  where toFile b = File <$> (b ^? bkey "length" . binteger) <*> (over (_Just.each) (\(String s) -> s) (b ^? bkey "path" . blist))
