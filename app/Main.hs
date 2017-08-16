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
data Message = Message { kind :: Text, value :: Text, sender :: Text, target :: Maybe Text }
    deriving (Show, Generic)

-- Generates JSON string from components
f :: Text -> Text -> Text -> Maybe Text -> Text
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

invalidName :: Text -> Bool
invalidName username = any ($ username) [T.null, T.any isPunctuation, T.any isSpace]


main :: IO ()
main = do
    a <- newMVar newRoom
    b <- newMVar newRoom
    c <- newMVar newRoom
    let serverState = Map.fromList [("", a),
                                    ("#secret", b),
                                    ("#business", c)]
    print "Running on port 9160"
    WS.runServer "0.0.0.0" 9160 $ application serverState

application :: ServerState -> WS.ServerApp
application serverState pending = do
    conn <- WS.acceptRequest pending
    text <- WS.receiveData conn
    WS.forkPingThread conn 30

    case decode text :: Maybe Message of
        Nothing -> sendText "Could not decode opening message" ("", conn)
        Just message -> do

            let roomName = value message
            let username = sender message
            let client = (username, conn)

            case Map.lookup roomName serverState of
                Nothing -> sendText "That room is not available" ("", conn)
                Just room -> do

                    let connect = do
                            -- Append client and start talk loop
                            modifyMVar_ room $ \s -> do
                                let userList = T.intercalate ";" (map fst s)
                                let s' = addClient client s
                                sendText (f "login" userList "@server" Nothing) client
                                broadcast (f "connect" username "@server" Nothing) s'
                                return s'
                            talk room client

                    let disconnect = do
                            -- Remove client and return new state
                            s <- modifyMVar room $
                                \s -> let s' = removeClient client s in return (s', s')
                            broadcast (f "disconnect" username "@server" Nothing) s

                    clients <- readMVar room
                    case message of
                        _   | invalidName username ->
                                sendText "Name cannot contain punctuation or whitespace, and cannot be empty" client
                            | clientExists client clients ->
                                sendText "User already exists" client
                            | otherwise ->
                                flip finally disconnect $ connect

talk :: MVar RoomState -> Client -> IO ()
talk room (username, conn) = forever $ do
    text <- WS.receiveData conn
    case decode text :: Maybe Message of
        Nothing -> sendText "Error processing message" (username, conn)
        Just message -> case target message of
            Nothing -> readMVar room >>= broadcast json
            Just target -> readMVar room >>= sendPM json target
            where
                json = (f (kind message) (value message) (username) (target message))
