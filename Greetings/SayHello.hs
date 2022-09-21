module Greetings.SayHello where

  sayHello :: String -> IO ()
  sayHello x =
    putStrLn ("Hello, " ++ x ++ " and welcome to Haskell!")