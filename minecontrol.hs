{- LANGUAGE DeriveGeneric #-}

{-
minecontrol.hs - A library for interacting with an RCON enabled minecraft server
Copyright (C) 2015  Josh Proehl <josh@daedalusdreams.com>
***************************************************************************
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.
You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
***************************************************************************
-}


import Data.Int
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL -- (copy, toStrict, fromStrict)
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get
import           GHC.Generics (Generic)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

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

-}
data MCRConPacket = MCRConPacket {
       request_id       :: Int
     , request_type     :: Int
     , payload          :: String
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


{-
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


sendPacket :: Socket -> MCRConPacket -> IO MCRConPacket
sendPacket sock pkt = do
  sendAll sock (BL.toStrict (encode pkt))
  readPacket sock

readPacket :: Socket -> IO MCRConPacket
readPacket sock = do
  lenBytes <- recv sock 4
  let len = fromIntegral $ readInt32 $ BL.fromStrict lenBytes
  packetByte <- recv sock len
  let retPkt = (decode (BL.fromStrict packetByte)) :: MCRConPacket
  return retPkt



main :: IO ()
main = withSocketsDo $ do

  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "25575")
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  -- Mark the socket for keep-alive handling since it may be idle
  -- for long periods of time
  setSocketOption sock KeepAlive 1

  -- Connect to server
  connect sock (addrAddress serveraddr)




  -- TESTING CODE BELOW:

  let sesskey = 1965::Int
  let authPacket = MCRConPacket sesskey 3 "password"

  putStrLn "Sending auth packet:"
  authResp <- sendPacket sock authPacket

  putStrLn "Auth Response was: "
  print authResp

  let cmdPacket = MCRConPacket sesskey 2 "list"
  cmdResp <- sendPacket sock cmdPacket
  putStrLn "Command response was: "
  print cmdResp

  sClose sock
