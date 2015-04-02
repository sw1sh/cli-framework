module CLI.Framework.Application
    ( Application(..)
    , defaultMain
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Text (Text(..))
import qualified Data.Text as T
import System.Console.GetOpt (OptDescr)
import System.Console.Haskeline
import System.Console.Haskeline.MonadException (MonadException(..))

import qualified CLI.Framework.Command as C
import CLI.Framework.Command (Command(..))

type Commands m env = [Command m env]
type Options opts = [OptDescr opts]

data Application m env opts =
    Application {
    -- | Name of the application
      name                :: Text

    -- | Software version
    , version             :: Text

    -- | Authors
    , authors             :: [Text]

    -- | Description of the application
    , description         :: Text

    -- | Commands
    , commands            :: Commands m env

    -- | Options accepted by the application
    , options             :: Options opts
}

instance MonadIO m => Default (Application m env opts) where
    def = Application "" "0.0" [] "" (map (\c -> c { C.handler = \x y -> liftIO $ C.handler c x y }) [C.quit]) []

defaultMain :: forall env opts. Default env => Application (StateT env IO) env opts -> IO ()
defaultMain Application{..} = flip evalStateT def $ runInputT defaultSettings loop
    where
    loop :: InputT (StateT env IO) ()
    loop =
        getInputLine "% " >>= \case
            Nothing -> loop
            Just input -> case T.words $ T.pack input of
                (commandName:arguments) -> lift (execCommand commands commandName arguments) >> loop
                _                   -> loop

execCommand :: MonadIO m => [Command (StateT a m) a] -> Text -> [Text] -> StateT a m ()
execCommand (cmd:cmds) cmdName args
      | C.name cmd == cmdName = get >>= C.handler cmd args >>= put
      | otherwise             = execCommand cmds cmdName args
execCommand [] _ _ = liftIO $ print "No command found"
