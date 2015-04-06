{-
Use the Network.Minecraft.RCON library to create an interactive shell console
for accessing a minecraft server.

Uses the low level networking primitives to simply connect to the server and
send commands, printing the resulting string.

Usage:
  ./console <server-url> <port> <password>

-}
module Main where

import Network.Minecraft.RCON.Client
import System.Environment (getArgs)
import System.Exit
import System.IO

usage = hPutStrLn stderr $ "Usage: console <server-url> <port> <password>"
exit  = exitWith ExitSuccess
die   = exitWith $ ExitFailure 1
closeExit conn = mcCloseConnection conn >> exit

parseArgs :: [String] -> IO [String]
parseArgs ["-h"]   = usage >> exit
parseArgs args     = return args

runConsole :: [String] -> IO ()
runConsole tArgs = do
  let numArgs = length tArgs
  case numArgs of
    3 -> do
        hPutStrLn stdout "Ctrl-D or type \"quit\" to exit."
        conn <- mcGetConnection (tArgs !! 0) (tArgs !! 1) (tArgs !! 2)
        doConsole conn
    _ -> do
        usage
        exit


doConsole conn = do
  hPutStr stdout "> "
  hFlush stdout
  l <- getLine
  case l of
    "stop" -> do
        retPkt <- mcCommand conn "stop"
        hPutStrLn stdout retPkt
        hPutStrLn stdout "Assuming server stopped, closing remote control connection"
        closeExit conn
    "quit" -> do
        hPutStrLn stdout "Closing remote control. Server remains running."
        closeExit conn
    cmd -> do
        retPkt <- mcCommand conn cmd
        hPutStrLn stdout retPkt
        doConsole conn

main :: IO ()
main = getArgs >>= parseArgs >>= runConsole
