{-# LANGUAGE OverloadedStrings #-}
module Torrent.MetainfoSpec (spec) where

  import Data.Map (fromList)
  import Control.Monad

  import Test.Hspec

  import Torrent.Metainfo
  import Bencode

  spec :: Spec
  spec = do
    describe "files" $ do
      it "works" $ do
        let actual = files [Dict $ fromList [("length", Integer 1), ("path", List [String "AAAA"]) ]]
        let expect = Just [File 1 ["AAAA"]]
        actual `shouldBe` expect
      it "works2" $ do
        let actual = files [Dict $ fromList [("length", Integer 1), ("path", List [String "AAAA"])], Dict $ fromList [("length", Integer 3), ("path", List [String "BBBB"])]]
        let expect = Just [File 1 ["AAAA"], File 3 ["BBBB"]]
        actual `shouldBe` expect

    describe "parseInfoDict" $ do
      it "works" $ do
        let expect = Just $ Info "test" 1 ["test"] [File 1 ["test"]] "hash"
        let actual = parseInfoDict . Dict $ fromList [("name", String "test"), ("piece length",Integer 1), ("pieces", String "test"), ("files", List [Dict $ fromList [("length", Integer 1), ("path", List [String "test"])]])]
        actual `shouldBe` expect

    describe "parseMetainfo" $ do
      it "parses correctly" $ do
        let expect = Just $ Metainfo "tracker_url" $ Info "name" 1 ["piece"] [File 1 ["path"]] "hash"
        let actual = parseMetainfo . Dict $ fromList [("announce", String "tracker_url"), ("info", Dict $ fromList [("name", String "name"), ("piece length",Integer 1), ("pieces", String "piece"), ("files", List [Dict $ fromList [("length", Integer 1), ("path", List [String "path"]) ]]) ])]
        actual `shouldBe` expect
