{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
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
type RoomState = [Client]
type ServerState = Map.Map Text (MVar RoomState)

newRoom :: RoomState
newRoom = []

clientExists :: Client -> RoomState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> RoomState -> RoomState
addClient client clients = client : clients

removeClient :: Client -> RoomState -> RoomState
removeClient client = filter ((/= fst client) . fst)

sendText :: Text -> Client -> IO ()
sendText text (username, conn) = do WS.sendTextData conn text

broadcast :: Text -> RoomState -> IO ()
broadcast text clients = do forM_ clients $ sendText text

sendPM :: Text -> Text -> RoomState -> IO ()
sendPM text target clients = do forM_ clients' $ sendText text
    where clients' = filter ((== target) . fst) clients


main :: IO ()
main = do
    a <- newMVar newRoom
    b <- newMVar newRoom
    c <- newMVar newRoom
    let serverState = Map.fromList [("room1", a),
                                    ("room2", b),
                                    ("room3", c)]
    print "Running on port 9160"
    WS.runServer "0.0.0.0" 9160 $ application serverState

application :: ServerState -> WS.ServerApp
application serverState pending = do
    conn <- WS.acceptRequest pending
    text <- WS.receiveData conn
    WS.forkPingThread conn 30

    case Map.lookup "room1" serverState of
        Nothing -> sendText "<room1> is not available" ("?", conn)
        Just room -> do
            clients <- readMVar room

            case decode text :: Maybe Message of
                Nothing -> sendText "Connection error" ("", conn)
                Just message ->

                    case message of
                    _   | any ($ username) [T.null, T.any isPunctuation, T.any isSpace] ->
                            sendText "Name cannot contain punctuation or whitespace, and cannot be empty" client
                        | clientExists client clients ->
                            sendText "User already exists" client
                        | otherwise ->

                                flip finally disconnect $ do
                                modifyMVar_ room $ \s -> do
                                    let userList = T.intercalate ";" (map fst s)
                                    sendText (f "login" userList Nothing Nothing) client
                                    let s' = addClient client s
                                    broadcast (f "connect" username Nothing Nothing) s'
                                    return s'
                                talk room client

                    where
                        username = value message
                        client = (username, conn)
                        disconnect = do
                            -- Remove client and return new state
                            s <- modifyMVar room $
                                \s -> let s' = removeClient client s in return (s', s')
                            broadcast (f "disconnect" username Nothing Nothing) s

talk :: MVar RoomState -> Client -> IO ()
talk room (username, conn) = forever $ do
    text <- WS.receiveData conn
    case decode text :: Maybe Message of
        Nothing -> sendText "Error processing message" (username, conn)
        Just message -> case target message of
            Nothing -> readMVar room >>= broadcast json
            Just target -> readMVar room >>= sendPM json target
            where
                json = (f (kind message) (value message) (Just username) (target message))
