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
type Channel = [Client]
type Server = Map.Map Text (MVar Channel)

newChannel :: Channel
newChannel = []

clientExists :: Client -> Channel -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> Channel -> Channel
addClient client channel = client : channel

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
    case Map.lookup channelName server of
        Just channelReference ->
            return channelReference

        Nothing -> do
            channelReference <- newMVar newChannel
            modifyMVar_ serverReference $ \s -> do
                let s' = Map.insert channelName channelReference s
                return s'
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
                    channel <- modifyMVar channelReference $
                        \s -> let s' = addClient client s in return (s', s')
                    let userList = T.intercalate ";" (map fst channel)
                    sendText (f "login" userList "@server" Nothing) client
                    broadcast (f "connect" username "@server" Nothing) channel
                    talk channelReference client

            let disconnect = do
                    -- Remove client and return new state
                    channel <- modifyMVar channelReference $
                        \s -> let s' = removeClient client s in return (s', s')
                    broadcast (f "disconnect" username "@server" Nothing) channel
                    -- Remove channelReference if empty
                    let x = if null channel then Nothing else Just channelReference
                    modifyMVar_ serverReference $
                        \s -> let s' = Map.update (const x) channelName s in return s'

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
