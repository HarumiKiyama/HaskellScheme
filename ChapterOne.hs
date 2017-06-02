module Main where
import           System.Environment

main::IO()
main = do putStrLn "What is your name"
          args<-getLine
          putStrLn ("Hello, " ++ args)
