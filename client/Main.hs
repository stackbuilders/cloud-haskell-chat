module Main where

import Client
import Options.Applicative
import Data.Monoid ((<>))
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)


-- | Data type representing collection of options that the program accepts.

data Options = Options
  { serverAddr :: Maybe String
  , host :: Maybe String
  , port :: Maybe Int
  , chatName :: Maybe String
  }


main :: IO ()
main = do
  opts <- liftIO (execParser parserInfo)
  case opts of
    Options Nothing _ _ _ -> putStrLn "Please, provide the server's ADDRESS ... "
    Options _ Nothing _ _ -> putStrLn "Please, provide the client's HOST ... "
    Options _ _ Nothing _ -> putStrLn "Please, provide the client's PORT ... "
    Options _ _ _ Nothing -> putStrLn "Please, provide the client's chat-room NAME ... "
    Options (Just addr) (Just h) (Just prt) (Just name) -> launchChatClient addr h prt name


-- | Info that command line option parser needs to work.

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionParser)
  ( fullDesc <>
    progDesc "Chat client ready to connect to a server ... " <>
    header "chat-client — chat client powered by Cloud Haskell ... "     )

-- | Description of command line options

optionParser :: Parser Options
optionParser = Options
  <$> option (Just <$> str)
  ( long     "address"  <>
    metavar  "ADDRESS"  <>
    value    Nothing    <>
    help
    "The chat server's address." )
  <*> option (Just <$> str)
  ( long     "host"   <>
    metavar  "HOST"   <>
    value    Nothing  <>
    help
    "The chat client's host." )
  <*> option (str >>= parsePort)
  ( long     "port"   <>
    metavar  "PORT"   <>
    value    Nothing  <>
    help
    "The chat client's port." )
   <*> option (Just <$> str)
  ( long     "room"      <>
    metavar  "ROOMNAME"  <>
    value    Nothing     <>
    help
    "The chat-room we want to connect to." )
  where
    parsePort :: Monad m => String -> m (Maybe Int)
    parsePort = pure . readMaybe
