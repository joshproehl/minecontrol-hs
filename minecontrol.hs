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
-- import qualified Data.ByteString       as B
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL (copy, toStrict, fromStrict)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import GHC.Generics (Generic)

import           Network.Socket
import qualified Network.Socket.ByteString as BS

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
data Packet = Packet {
       request_length   :: Int
     , request_id       :: Int
     , request_type     :: Int
     , payload          :: String
} deriving (Show)

-- Make it possible to encode/decode Packets from binary format.
instance Binary Packet where
  put (Packet len id tp payload) = do
      putWord32le $ fromIntegral len
      putWord32le $ fromIntegral id
      putWord32le $ fromIntegral tp
      putByteString (BC.pack payload)
      -- Put the two final buffer chars
      putWord8 0
      putWord8 0

  get = do
          len     <- getWord32le
          id      <- getWord32le
          tp      <- getWord32le
          payload <- getByteString ((fromIntegral len) - 8)    -- We've already parsed the 2nd and 3rd fields, and the len is only for AFTER the first field

          -- TODO: Ensure the last two are the null bytes

          return (Packet (fromIntegral len) (fromIntegral id) (fromIntegral tp) (BC.unpack payload))


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


sendPacket :: Socket -> Packet -> IO ()
sendPacket sock pkt = do
  BS.sendAll sock (BL.toStrict (encode pkt))
  --rMsg <- BS.recv sock 1024
  --let r = (decode (BL.fromStrict rMsg)) :: Packet
  --putStrLn $ "SendPacket Response: \n  " ++ (show r)
  --return (decode (BL.fromStrict rMsg)) ::Packet
  return ()


main :: IO ()
main = do

  let sesskey = 1965::Int
  let authPacket = Packet 10 sesskey 3 ""

  print "Auth Packet is:"
  print authPacket

  sock <- socket AF_INET Stream defaultProtocol
  connect sock addr

  sendPacket authPacket
