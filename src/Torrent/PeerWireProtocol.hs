module Torrent.PeerWireProtocol where

  import Data.Binary
  import Data.Binary.Get (getByteString)
  import Data.Binary.Put (putByteString)

  import Data.ByteString as B

  data Message
    = Choke
    | Unchoke
    | Interested
    | NotInterested
    | Have Word32
    | Bitfield ByteString -- ???
    | Request Word32 Word32 Word32
    | Piece Word32 Word32 ByteString
    | Cancel Word32 Word32 Word32
    | Keepalive
    deriving (Show, Eq)

  mId :: Message -> Word8
  mId Choke = 0
  mId Unchoke = 1
  mId Interested = 2
  mId NotInterested = 3
  mId (Have _) = 4
  mId (Bitfield _) = 5
  mId (Request _ _ _) = 6
  mId (Piece _ _ _) = 7
  mId (Cancel _ _ _) = 8


  instance Binary Message where
    put Keepalive = put (0 :: Word32)
    put a@(Have w) = do
      put (5 :: Word32)
      put $ mId a
      put w
    put a@(Bitfield field) = do
      put $ (fromIntegral $ 1 + (B.length field) :: Word32)
      put $ mId a
      putByteString field
    put a@(Request idx beg len) = do
      put (1 + 12 :: Word32)
      put $ mId a
      mapM_ put [idx, beg, len]
    put a@(Piece idx beg blk) = do
      put (fromIntegral $ 9 + (B.length blk) :: Word32)
      put $ mId a
      mapM_ put [idx, beg]
      putByteString blk
    put a@(Cancel idx beg len) = do
      put (1 + 12 :: Word32)
      put $ mId a
      mapM_ put [idx, beg, len]
    put a = do
      put (1 :: Word32)
      put $ mId a

    get = do
      len <- get :: Get Word32
      case len of
        0 -> return Keepalive
        _ -> do
          id <- get :: Get Word8
          case id of
            0 -> return Choke
            1 -> return Unchoke
            2 -> return Interested
            3 -> return NotInterested
            4 -> Have <$> get
            5 -> Bitfield <$> getByteString (fromIntegral len - 1)
            6 -> Request <$> get <*> get <*> get
            7 -> Piece <$> get <*> get <*> getByteString (fromIntegral len - 9)
            8 -> Cancel <$> get <*> get <*> get
            _ -> fail "invalid message"
