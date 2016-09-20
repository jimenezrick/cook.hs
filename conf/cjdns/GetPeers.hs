import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import System.FilePath.Find

import qualified Data.ByteString.Lazy as B

import Cook.Recipe
import Cook.Recipe.Config

main :: IO ()
main = do
    runRecipe $ withRecipeName "GetPeers" $ do
        runSh "curl -s -L https://github.com/hyperboria/peers/archive/master.tar.gz | tar xz"
        peerFiles <- liftIO $ find always (extension ==? ".k") "peers-master"

        liftIO $ putStrLn "Loading peers:"
        liftIO $ forM_ peerFiles putStrLn

        peers <- catMaybes <$> (liftIO $ mapM (B.readFile >=> return . decode) peerFiles)
        writeConfig "peers.json" (foldl1 mergeConfig peers)

        runProc "rm" ["-rf", "peers-master"]
