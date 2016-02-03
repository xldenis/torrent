{-# LANGUAGE OverloadedStrings #-}
module Torrent.PeerWireProtocolSpec where

  import Torrent.PeerWireProtocol


  import Test.Hspec

  import Data.ByteString.Char8 (unpack)
  import Data.Binary (encode, decode)

  spec :: Spec
  spec = do
    describe "binary parser laws hold" $ do
      it "Choke serializes and deserializes" $ do
        (decode $ encode Choke) `shouldBe` Choke
      it "Request serializes and deserializes" $ do
        (decode . encode $ Request 1 1 1) `shouldBe` (Request 1 1 1)
      it "Bitfield serializes and deserializes" $ do
        (decode . encode $ Bitfield "test") `shouldBe` (Bitfield "test")
