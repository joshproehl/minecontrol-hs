--
--
--------
import Network.Minecraft.RCON.Client

main :: IO ()
main = do
  conn <- mcGetConnection "127.0.0.1" "25575" "password"
  putStrLn "Executing list command:"
  retPkt <- mcCommand conn "list"
  print retPkt
  mcCloseConnection conn

