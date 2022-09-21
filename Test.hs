module Test where
  
  import System.TimeIt
  import Greetings.SayHello

  main :: IO ()
  main = timeItNamed "sayhello" $ sayHello "Maat"

  


