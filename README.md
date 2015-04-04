# Minecontrol-HS
Sometimes you want to poke at or interact with your minecraft server without logging in. Sometimes from inside a Haskell app. Well here we are!

## Example Usage


Access the server and issue commands directly using the low level connection primitives:
  ```haskell
  import Network.MineCraft.MCRCon

  main :: IO ()
  main = do
    conn <- mcGetConnection "127.0.0.1" "25575" "password"
    putStrLn "Executing list command:"
    retPkt <- mcCommand conn "list"
    print retPkt
    mcCloseConnection conn
  ```
