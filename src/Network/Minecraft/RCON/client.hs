--------------------------------------------------------------------------------
-- |
--  Module      :  Client
--  License     :  GPL
--
--  Maintainer  :  Josh Proehl
--  Stability   :  experimental
--  Portability :  
--
-- The main interface for connecting to the Minecraft RCON server.
-- Exports the minimum required for a connection, sending commands as strings,
-- and getting back results as strings.
--
-- For example usage see the README.md file or examples/Console.hs
--
--------------------------------------------------------------------------------

module Network.Minecraft.RCON.Client
  (
    MCRConHandle
  , MCRConPacket
  , mcGetConnection
  , mcCloseConnection
  , mcCommand
  ) where

import Network.Minecraft.RCON.Client.Internal

