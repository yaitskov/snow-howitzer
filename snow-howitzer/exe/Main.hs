module Main where

import ShowHowitzer.Cli qualified as Cli

import Relude

main :: IO ()
main = Cli.parseCmdArgs >>= Cli.runSnowHowitzerCommand
