module SnowHowitzer.Cli where

import SnowHowitzer.Cmd.RunServer
import SnowHowitzer.Lib
import SnowHowitzer.Types
import Control.Monad.Reader
import Options.Applicative
import Relude

newtype SnowHowitzerCommand = RunServer HttpAppPort deriving (Show, Eq)

backEndPortOption :: Parser HttpAppPort
backEndPortOption = HttpAppPort <$>
  option
    auto (short 'p'
            <> long "port"
            <> value 8182
            <> showDefault
            <> help "HTTP backend port to listen to")


cmdArgsParser :: Parser SnowHowitzerCommand
cmdArgsParser = RunServer <$> backEndPortOption

parseCmdArgs :: IO SnowHowitzerCommand
parseCmdArgs =
  execParser
    (info
      (cmdArgsParser <**> helper)
      (progDesc "SnowHowitzer")
    )

runSnowHowitzerCommand :: SnowHowitzerCommand -> IO ()
runSnowHowitzerCommand = \case
  RunServer  backPort ->
    mkAppSt >>=
      runReaderT (runServerCmd backPort)
