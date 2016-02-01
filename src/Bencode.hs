{-# LANGUAGE Rank2Types #-}
module Bencode where

import Data.Attoparsec.ByteString as P hiding (string)
import Data.Attoparsec.ByteString.Char8 hiding (string)

import Data.ByteString
import Data.Map as M

import Control.Applicative ((<|>))

import Control.Lens.Traversal

data Bencode
  = String ByteString
  | Integer Integer
  | List [Bencode]
  | Dict (Map ByteString Bencode)
  deriving Show

string :: Parser Bencode
string = do
  len <- decimal
  char ':'
  String <$> P.take len

integer :: Parser Bencode
integer = do
  char 'i'
  val <- decimal
  char 'e'
  return $ Integer val

list :: Parser Bencode
list = do
  char 'l'
  elems <- many' bencode <* char 'e'
  return $ List elems

dict :: Parser Bencode
dict = do
  char 'd'
  elems <- many' $ (,) <$> string <*> bencode
  char 'e'
  return . Dict . fromList $ fixPair <$> elems
  where fixPair (String s, v) = (s,v)

bencode :: Parser Bencode
bencode = string <|> integer <|> list <|> dict

bstring :: Traversal' Bencode ByteString
bstring f (String s) = String <$> f s
bstring _ bv = pure bv

binteger :: Traversal' Bencode Integer
binteger f (Integer i) = Integer <$> f i
binteger f b = pure b

blist :: Traversal' Bencode [Bencode]
blist f (List l) = List <$> f l
blist _ b = pure b

bkey :: ByteString -> (Traversal' Bencode Bencode)
bkey k f b@(Dict m) = case M.lookup k m of
  Nothing -> pure b
  Just v -> f v
bkey _ _ b = pure b
