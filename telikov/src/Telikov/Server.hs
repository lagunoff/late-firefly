module Main where
import Haste.App 
import Telikov.RPC

main :: IO ()
main = do
  runApp [start (Proxy :: Proxy MyS)] $ pure ()
