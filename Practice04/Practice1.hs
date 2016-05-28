module Main where

type Key = Int
type Value = Int
data Command = CInput Key Value | CFind Key | CQuit deriving (Read)
type DB = [(Key, Value)]

main :: IO ()
main = do
  database []

database :: DB -> IO ()
database l = do
  x <- getLine
  let com = read x in
    dbCommand com l

dbCommand :: Command -> DB -> IO ()
dbCommand com l = case com of
      CInput k v -> database ((k,v):l)
      CFind k -> dbFind k l >> database l
      CQuit -> putStrLn "Good bye!"

dbFind :: Key -> DB -> IO ()
dbFind k l = case dropWhile (\(kx, _) -> k /= kx) l of
  [] -> putStrLn "Error = Key Not Found."
  (_, vx):_ -> putStrLn . show $ vx
