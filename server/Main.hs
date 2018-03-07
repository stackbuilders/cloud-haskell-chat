module Main where

import Server
import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Options.Applicative
import Control.Monad.IO.Class (liftIO)


-- | Data type representing collection of options that the program accepts.

data Options = Options
  { serverHost :: Maybe String
  , port :: Maybe Int
  , chatName :: Maybe String
  }


main :: IO ()
main = do
  opts <- liftIO (execParser parserInfo)
  case opts of
    Options Nothing _ _ -> putStrLn "Please, provide the server's HOST ... "
    Options _ Nothing _ -> putStrLn "Please, provide the server's PORT ... "
    Options _ _ Nothing -> putStrLn "Please, provide the server's chat-room NAME ... "
    Options (Just host) (Just prt) (Just name) -> serveChatRoom host prt name


-- | Info that command line option parser needs to work.

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionParser)
  ( fullDesc <>
    progDesc "Chat server ready to listen for client connections ... " <>
    header "chat-server — chat server powered by Cloud Haskell ... "     )


-- | Description of command line options

optionParser :: Parser Options
optionParser = Options
  <$> option (Just <$> str)
  ( long     "host"    <>
    metavar  "HOST"    <>
    value    Nothing   <>
    help
    "The chat server's host." )
  <*> option (str >>= parsePort)
  ( long     "port"   <>
    metavar  "PORT"   <>
    value    Nothing  <>
    help
    "The chat server's port." )
   <*> option (Just <$> str)
  ( long     "room"      <>
    metavar  "ROOMNAME"  <>
    value    Nothing     <>
    help
    "The name for this chat-room." )
  where
    parsePort :: Monad m => String -> m (Maybe Int)
    parsePort = pure . readMaybe
