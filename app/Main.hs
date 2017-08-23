{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import GHC.Generics (Generic)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS

-- https://artyom.me/aeson
-- https://github.com/bos/aeson/blob/master/examples/Generic.hs
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
data Message = Message { kind :: Text, value :: Text, sender :: Text, target :: Maybe Text }
    deriving (Show, Generic)

-- Generates JSON string from components
f :: Text -> Text -> Text -> Maybe Text -> Text
f kind value sender target = (toStrict $ decodeUtf8 $ encode message)
    where message = Message { kind = kind, value = value, sender = sender, target = target }

instance FromJSON Message
instance ToJSON Message

type Client = (Text, WS.Connection)
type Channel = [Client]
type Server = Map.Map Text (MVar Channel)

dup :: a -> (a, a)
dup x = (x, x)

newChannel :: Channel
newChannel = []

addClient :: Client -> Channel -> Channel
addClient = (:)

clientExists :: Client -> Channel -> Bool
clientExists client = any ((== fst client) . fst)

removeClient :: Client -> Channel -> Channel
removeClient client = filter ((/= fst client) . fst)

sendText :: Text -> Client -> IO ()
sendText text (username, conn) = do WS.sendTextData conn text

broadcast :: Text -> Channel -> IO ()
broadcast text channel = do forM_ channel $ sendText text

sendPM :: Text -> Text -> Channel -> IO ()
sendPM text target channel = do forM_ channel' $ sendText text
    where channel' = filter ((== target) . fst) channel

invalidName :: Text -> Bool
invalidName username = any ($ username) [T.null, T.any isPunctuation, T.any isSpace]

getOrCreateChannel :: Text -> MVar Server -> IO (MVar Channel)
getOrCreateChannel channelName serverReference = do
    server <- readMVar serverReference
    maybe channelReference return (Map.lookup channelName server)
        where channelReference = createChannel channelName serverReference

createChannel :: Text -> MVar Server -> IO (MVar Channel)
createChannel channelName serverReference = do
    channelReference <- newMVar newChannel
    modifyMVar_ serverReference (return . Map.insert channelName channelReference)
    return channelReference

main :: IO ()
main = do
    serverReference <- newMVar Map.empty
    print "Running on port 9160"
    WS.runServer "0.0.0.0" 9160 $ application serverReference

application :: MVar Server -> WS.ServerApp
application serverReference pending = do
    conn <- WS.acceptRequest pending
    text <- WS.receiveData conn
    WS.forkPingThread conn 30

    case decode text :: Maybe Message of
        Nothing -> sendText "Could not decode opening message" ("", conn)
        Just message -> do

            let channelName = value message
            let username = sender message
            let client = (username, conn)
            channelReference <- getOrCreateChannel channelName serverReference

            let connect = do
                    -- Append client and start talk loop
                    channel <- modifyMVar channelReference (return . dup . addClient client)
                    let userList = T.intercalate ";" (map fst channel)
                    sendText (f "login" userList "@server" Nothing) client
                    broadcast (f "connect" username "@server" Nothing) channel
                    talk channelReference client

            let disconnect = do
                    -- Remove client and return new state
                    channel <- modifyMVar channelReference (return . dup . removeClient client)
                    broadcast (f "disconnect" username "@server" Nothing) channel
                    -- Remove channelReference if empty
                    let val = if null channel then Nothing else Just channelReference
                    modifyMVar_ serverReference (return . Map.update (const val) channelName)

            channel <- readMVar channelReference
            case message of
                _   | invalidName username ->
                        sendText "Name cannot contain punctuation or whitespace, and cannot be empty" client
                    | clientExists client channel ->
                        sendText "User already exists" client
                    | otherwise ->
                        finally connect disconnect

talk :: MVar Channel -> Client -> IO ()
talk channelReference (username, conn) = forever $ do
    text <- WS.receiveData conn
    case decode text :: Maybe Message of
        Nothing -> sendText "Error processing message" (username, conn)
        Just message -> case target message of
            Nothing -> readMVar channelReference >>= broadcast json
            Just target -> readMVar channelReference >>= sendPM json target
            where
                json = f (kind message) (value message) (username) (target message)
