module Main where

import Options.Applicative
import Data.Text as T
import Error
import Parser
import Semant

data Action = Ast | Sast deriving (Eq)

data Options = Options { action :: Action
                       , input :: String
                       }

actionP :: Parser Action
actionP = flag' Ast (long "ast" <> short 'a' <> help "Print the AST")
      <|> flag' Sast (long "sast" <> short 's' <> help "Print the SAST")

optionsP :: Parser Options
optionsP = Options <$> actionP <*> strArgument (help "Input")

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString = "Run the cajon compiler on the given input."

runOpts :: Options -> IO ()
runOpts (Options action input) = do
  case runParser programP "" (T.pack input) of
    Left  err -> putStrLn $ "Parse error: " ++ (show err)
    Right ast -> do
      if action == Ast
         then putStrLn $ show ast
         else
          case checkProgram ast of
            Left err   -> putStrLn $ formatSemantError err
            Right sast -> putStrLn $
              if action == Sast
                 then show sast
                 else "Finished."
