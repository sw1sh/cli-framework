module CLI.Framework.Command
    ( Command(..),
      quit
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Text
import System.Exit

-- | It is often simpler to use the default implementation of Command, and
-- override it with the details you choose to use.
--
data Command m env = Command {
    -- | Name of the command
      name          :: Text

    -- | Handler
    , handler       :: [Text] -> env -> m env

    -- | Description
    , description   :: Text
}

instance Applicative m => Default (Command m env) where
    def = Command "<Uknown command>" (const pure) ""

defCmd = def :: forall env. Command IO env

quit = defCmd { name = "quit", handler = quitHandler, description = "exit the application" }
    where
    quitHandler :: [Text] -> env -> IO env
    quitHandler _ env = exitSuccess >> return env
