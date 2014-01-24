import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.Lifted.Fork
import Control.Monad.State.Concurrent
import Network
import System.IO

data Client = Client
            { clientId :: Int
            , clientHandle :: Handle
            } deriving Show

data ServerState = ServerState
                 { clientNum :: Int
                 , clients :: [Client]
                 } deriving Show

type ServerEnv = StateC ServerState IO

runClient :: Client -> ServerEnv ()
runClient client = forever $ do
    m <- liftIO $ hGetLine (clientHandle client)
    let msg = "From client #" ++ show (clientId client) ++ ": " ++ m
    broadcastFrom (clientId client) msg

broadcastFrom :: Int -> String -> ServerEnv ()
broadcastFrom cid str = do
    liftIO $ putStrLn str
    clientList <- gets clients
    let otherClients = filter (\c -> clientId c /= cid) clientList
    forM_ otherClients $ \client ->
        liftIO $ hPutStrLn (clientHandle client) str

nextClientId :: ServerEnv Int
nextClientId = state $ \s -> (clientNum s, s { clientNum = clientNum s + 1 })

main :: IO ()
main = do
    server <- listenOn (PortNumber 12345)
    putStrLn "Listening on ::1:12345..."
    tv <- newTVarIO $ ServerState 0 []
    flip evalStateC tv $ forever $ do
        (ch, _, _) <- liftIO $ accept server
        void $ fork $ do
            i <- nextClientId
            liftIO $ putStrLn $ "Client #" ++ show i ++ " has joined."
            let client = Client i ch
            modify (\serverState -> serverState { clients = client : clients serverState })
            runClient client
