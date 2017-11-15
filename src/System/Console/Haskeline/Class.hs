{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

{- |

Module      :  System.Console.Haskeline.Class
Copyright   :  (c) Antoine Latter, 2009
License     :  BSD3
Maintainer  :  Antoine Latter <aslatter@gmail.com>
Stability   :  experimental
Portability :  FlexibleInstances, MultiPatamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving

Haskeline provides all of its functionality within the scope of a monad transformer.
This module adds two pieces to this:

- Introduced here is a type-class which defines the operations
   supported by the Haskeline monad transformer - MonadHaskeline

- A newtype wrapper around Haskeline's InputT, called
   HaskelineT. Sadly, InputT defines ints own instance of the
   mtl MonadState, which is no good for folks wanting to use
   InputT in an existing monad transformer stack.

   HaskelineT also has an instance of MonadState, but it merely
   lifts the functions further in the transformer stack.

Large portions of the Haskeline functionality are re-exported
here for convinience.

-}



module System.Console.Haskeline.Class
    (HaskelineT
    ,runHaskelineT
    ,runHaskelineTWithPrefs
    ,MonadHaskeline(..)
    ,H.Settings(..)
    ,H.defaultSettings
    ,H.setComplete
    ,H.Prefs()
    ,H.readPrefs
    ,H.defaultPrefs
    ,H.Interrupt(..)
    ,H.handleInterrupt
    ,module System.Console.Haskeline.Completion
    ,module System.Console.Haskeline.MonadException
     ) where

import qualified System.Console.Haskeline as H
import System.Console.Haskeline.Completion
import System.Console.Haskeline.MonadException

import Control.Applicative
import Control.Monad.State

newtype HaskelineT m a = HaskelineT {unHaskeline :: H.InputT m a}
 deriving (Monad, Functor, Applicative, MonadIO, MonadException, MonadTrans, MonadHaskeline)

-- | Run a line-reading application, reading user 'Prefs' from
-- @~/.haskeline@
runHaskelineT :: MonadException m => H.Settings m -> HaskelineT m a -> m a
runHaskelineT s m = H.runInputT s (unHaskeline m)

runHaskelineTWithPrefs :: MonadException m => H.Prefs -> H.Settings m -> HaskelineT m a -> m a
runHaskelineTWithPrefs p s m = H.runInputTWithPrefs p s (unHaskeline m)

class MonadException m => MonadHaskeline m where
    getInputLine :: String -> m (Maybe String)
    getInputChar :: String -> m (Maybe Char)
    outputStr :: String -> m ()
    outputStrLn :: String -> m ()

--deriving instance MonadHaskeline IO


instance MonadException m => MonadHaskeline (H.InputT m) where
    getInputLine = H.getInputLine
    getInputChar = H.getInputChar
    outputStr = H.outputStr
    outputStrLn = H.outputStrLn


instance MonadState s m => MonadState s (HaskelineT m) where
    get = lift get
    put = lift . put

instance MonadHaskeline m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

instance (MonadHaskeline m) => MonadHaskeline (StateT s m) where
    getInputLine = lift . getInputLine
    getInputChar = lift . getInputChar
    outputStr = lift . outputStr
    outputStrLn = lift . outputStrLn

instance MonadHaskeline IO where
    getInputLine = liftIO . getInputLine
    getInputChar = liftIO . getInputChar
    outputStr = liftIO . outputStr
    outputStrLn = liftIO . outputStrLn
