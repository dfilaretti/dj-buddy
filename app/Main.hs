module Main (main) where

import MyLib qualified (printLib)

main :: IO ()
main = MyLib.printLib
