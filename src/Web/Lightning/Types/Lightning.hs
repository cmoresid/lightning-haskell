{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Lightning.Types.Lightning where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Free
import qualified Data.Text                 as T

import           Data.Aeson

import           Network.API.Builder       hiding (runRoute)
import           Web.Lightning.Types.Error
import           Web.Lightning.Utilities

type Lightning a = LightningT IO a

data LightningF m a where
  FailWith     :: APIError LightningError -> LightningF m a
  ReceiveRoute :: Receivable b => Route -> (b -> a) -> LightningF m a
  RunRoute     :: FromJSON b => Route -> (b -> a) -> LightningF m a
  SendJSON     :: (Receivable b) => Value -> Route -> (b -> a) -> LightningF m a
  WithBaseURL  :: T.Text -> LightningT m b -> (b -> a) -> LightningF m a

instance Functor (LightningF m) where
  fmap _ (FailWith x)        = FailWith x
  fmap f (ReceiveRoute r x)  = ReceiveRoute r (fmap f x)
  fmap f (RunRoute r x)      = RunRoute r (fmap f x)
  fmap f (SendJSON js r x)   = SendJSON js r (fmap f x)
  fmap f (WithBaseURL u a x) = WithBaseURL u a (fmap f x)

newtype LightningT m a = LightningT (FreeT (LightningF m) m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans LightningT where
  lift = LightningT . lift

instance MonadIO m => MonadIO (LightningT m) where
  liftIO = LightningT . liftIO

runRoute :: (FromJSON a, Monad m) => Route -> LightningT m a
runRoute r = LightningT $ liftF $ RunRoute r id

sendPlot :: (ToJSON p, Receivable a, Monad m) => T.Text -> p -> Route -> LightningT m a
sendPlot t p r = LightningT $ liftF $ SendJSON (createPayLoad t $ toJSON p) r id

receiveRoute :: (Receivable a, Monad m) => Route -> LightningT m a
receiveRoute r = LightningT $ liftF $ ReceiveRoute r id

withBaseURL :: Monad m => T.Text -> LightningT m a -> LightningT m a
withBaseURL u f = LightningT $ liftF $ WithBaseURL u f id

failWith :: Monad m => APIError LightningError -> LightningT m a
failWith = LightningT . liftF . FailWith

defaultBaseURL :: T.Text
defaultBaseURL = "http://localhost:3000"
