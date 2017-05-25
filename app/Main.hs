{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Char (isPunctuation, isSpace)
import Data.Maybe (isJust, isNothing)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Network.WebSockets as WS

-- https://artyom.me/aeson
-- https://github.com/bos/aeson/blob/master/examples/Generic.hs
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
data Message = Message { kind :: Text, value :: Text, sender :: Maybe Text, target :: Maybe Text }
    deriving (Show, Generic)

-- Generates JSON string from components
f :: Text -> Text -> Maybe Text -> Maybe Text -> Text
f kind value sender target = (toStrict $ T.decodeUtf8 $ encode message)
    where message = Message { kind = kind, value = value, sender = sender, target = target }

instance FromJSON Message
instance ToJSON Message

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

sendText :: Text -> Client -> IO ()
sendText text (username, conn) = do WS.sendTextData conn text

broadcast :: Text -> ServerState -> IO ()
broadcast text clients = do forM_ clients $ sendText text

sendPM :: Text -> Text -> ServerState -> IO ()
sendPM text target clients = do forM_ clients' $ sendText text
    where clients' = filter ((== target) . fst) clients

main :: IO ()
main = do
    state <- newMVar newServerState
    print "Running on port 9160"
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    text <- WS.receiveData conn
    clients <- readMVar state
    case decode text :: Maybe Message of
        Nothing -> sendText "Connection error" ("", conn)
        Just message -> case message of
            _   | any ($ username) [T.null, T.any isPunctuation, T.any isSpace] ->
                    sendText "Name cannot contain punctuation or whitespace, and cannot be empty" client
                | clientExists client clients ->
                    sendText "User already exists" client
                | otherwise ->
                    flip finally disconnect $ do
                    modifyMVar_ state $ \s -> do
                        let userList = T.intercalate ";" (map fst s)
                        sendText (f "login" userList Nothing Nothing) client
                        let s' = addClient client s
                        broadcast (f "connect" username Nothing Nothing) s'
                        return s'
                    talk state client
                where
                    username = value message
                    client = (username, conn)
                    disconnect = do
                        -- Remove client and return new state
                        s <- modifyMVar state $
                            \s -> let s' = removeClient client s in return (s', s')
                        broadcast (f "disconnect" username Nothing Nothing) s

talk :: MVar ServerState -> Client -> IO ()
talk state (username, conn) = forever $ do
    text <- WS.receiveData conn
    case decode text :: Maybe Message of
        Nothing -> sendText "Error processing message" (username, conn)
        Just message -> case target message of
            Nothing -> readMVar state >>= broadcast json
            Just target -> readMVar state >>= sendPM json target
            where
                json = (f (kind message) (value message) (Just username) (target message))
