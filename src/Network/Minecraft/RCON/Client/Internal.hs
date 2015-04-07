--------------------------------------------------------------------------------
-- |
--  Module      :  Internal
--  License     :  GPL
--
--  Maintainer  :  Josh Proehl
--  Stability   :  experimental
--  Portability :  
--
-- The plumbing for the Mincraft RCON client.
--
--------------------------------------------------------------------------------

module Network.Minecraft.RCON.Client.Internal (module Network.Minecraft.RCON.Client.Internal) where

import           Data.Int
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL -- (copy, toStrict, fromStrict)
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import           System.Random


data MCRConHandle = MCRConHandle {
    mcrcon_sock     :: Socket
  , mcrcon_sesskey  :: Int
} deriving (Show)

{-
From wiki.vg/Rcon

Packet Format

Integers are little-endian, in contrast with the Beta protocol.
Responses are sent back with the same Request ID that you send.
In the event of an auth failure (i.e. your login is incorrect, or you're trying to send commands without first logging in), request ID will be set to -1

Field name  Field Type  Notes
Length      int         Length of remainder of packet
Request ID  int         Client-generated ID
Type        int         3 for login, 2 to run a command, 0 for a multi-packet response
Payload     byte[]      ASCII text
2-byte pad  byte, byte  Two null bytes

Packets

3: Login
  Outgoing payload: password.
  If the server returns a packet with the same request ID, auth was successful (note: packet type is 2, not 3). If you get an request ID of -1, auth failed (wrong password).
2: Command
  Outgoing payload should be the command to run, e.g. time set 0
0: Command response
  Incoming payload is the output of the command, though many commands return nothing, and there's no way of detecting unknown commands.
  The output of the command may be split over multiple packets, each containing 4096 bytes (less for the last packet). Each packet contains part of the payload (and the two-byte padding). The last packet sent is the end of the output.

Maximum request length: 1460 (giving a max payload length of 1446)
Code exists in the notchian server to split large responses (>4096 bytes) into multiple smaller packets. However, the code that actually encodes each packet expects a max length of 1248, giving a max response payload length of 1234 bytes.
-}
data MCRConPacket = MCRConPacket {
       mcrcon_request_id       :: Int
     , mcrcon_request_type     :: Int
     , mcrcon_payload          :: String
} deriving (Show)

-- Make it possible to encode/decode Packets from binary format.
instance Binary MCRConPacket where
  put (MCRConPacket id tp payload) = do
    let packedPayload = (BC.pack payload)
    let len = 8 + (BC.length packedPayload) + 2

    putWord32le $ fromIntegral len
    putWord32le $ fromIntegral id
    putWord32le $ fromIntegral tp
    putByteString packedPayload
    -- Put the two final buffer chars
    putWord8 0
    putWord8 0

  get = do
    id      <- getWord32le
    tp      <- getWord32le
    --payload <- getByteString ((fromIntegral len) - 10)    -- We've already parsed the 2nd and 3rd fields, and the len is only for AFTER the first field
    payload <- getLazyByteStringNul

    lastBuffer <- getWord8
    -- TODO: Ensure the last two are the null bytes
    -- TODO: Keep going into the next packet and accumulate their payloads if needed.

    return $ MCRConPacket (fromIntegral id) (fromIntegral tp) (BC.unpack (BL.toStrict payload))


readInt32 :: BL.ByteString -> Int
readInt32 s = runGet readIntGet s
            where
            readIntGet = do
                b <- getWord32le
                let c = (fromIntegral b)::Int
                return c


mcTransact :: MCRConHandle -> MCRConPacket -> IO MCRConPacket
mcTransact conn pkt = do
  sendAll (mcrcon_sock conn) (BL.toStrict (encode pkt))
  lenBytes <- recv (mcrcon_sock conn) 4
  let len = fromIntegral $ readInt32 $ BL.fromStrict lenBytes
  packetByte <- recv (mcrcon_sock conn) len
  let retPkt = (decode (BL.fromStrict packetByte)) :: MCRConPacket
  return retPkt


mcGetConnection :: String -> String -> String -> IO MCRConHandle
mcGetConnection url port password = withSocketsDo $ do
  addrinfos <- getAddrInfo Nothing (Just url) (Just port) -- TODO: give some default values? (127.0.0.1 and 25575)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  -- Mark the socket for keep-alive handling since it may be idle
  -- for long periods of time
  setSocketOption sock KeepAlive 1

  -- Connect to server
  connect sock (addrAddress serveraddr)

  -- Generate a sort-of-unique session key
  sesskey <- randomRIO(1,99999999::Int32)

  let conn = MCRConHandle sock (fromIntegral sesskey)

  authResponse <- mcTransact conn (MCRConPacket (fromIntegral sesskey) 3 password)

  -- TODO: Check auth response and make sure we authed properly, and only return the conn handle if it worked!

  return conn


mcCloseConnection :: MCRConHandle -> IO ()
mcCloseConnection conn = do
  sClose (mcrcon_sock conn)


mcCommand :: MCRConHandle -> String -> IO String
mcCommand conn cmd = do
  retPkt <- mcTransact conn (MCRConPacket (mcrcon_sesskey conn) 2 cmd)
  let str = mcrcon_payload retPkt
  return str


