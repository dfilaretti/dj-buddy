module Main (main) where

import CLI.Render
import Options.Applicative

data Command = Hello

commandParser :: Parser Command
commandParser =
  subparser $
    command
      "hello"
      ( info
          (pure Hello)
          (progDesc "Generate a sample playlist")
      )

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "DJ Buddy"
    )

main :: IO ()
main = do
  selectedCommand <- execParser parserInfo
  case selectedCommand of
    Hello -> showRandomPlaylist 10
