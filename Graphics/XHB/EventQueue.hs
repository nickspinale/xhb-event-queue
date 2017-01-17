{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}


module Graphics.XHB.EventQueue
    (  EventQueueT(..)
    , runEventQueueT
    , EventQueueCtx(..)
    , getEventQueue
    , getsEventQueue
    , putEventQueue
    , modifyEventQueue

    , flushEventQueue
    , pollEventQueue
    , waitEventQueue
    , takeEventsByType
    , skipEventsByType
    ) where


import Graphics.XHB
import Graphics.XHB.Monad

import Data.Foldable
import Data.Sequence (Seq, (<|), (|>))
import Data.Sequence as S
import Data.Typeable

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


type EventQueue = Seq SomeEvent

newtype EventQueueT m a = EventQueueT { unEventQueueT :: StateT EventQueue m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, Typeable)

deriving instance MonadX x m => MonadX x (EventQueueT m)

runEventQueueT :: MonadX x m => EventQueueT m a -> m a
runEventQueueT m = evalStateT (unEventQueueT m) S.empty


-- Class --

class Monad m => EventQueueCtx m where
    stateEventQueue :: (EventQueue -> (a, EventQueue)) -> m a

instance MonadX x m => EventQueueCtx (EventQueueT m) where
    stateEventQueue = EventQueueT . state

instance (EventQueueCtx m, MonadTrans t, Monad (t m)) => EventQueueCtx (t m) where
    stateEventQueue = lift . stateEventQueue


getEventQueue :: EventQueueCtx m => m EventQueue
getEventQueue = stateEventQueue $ \s -> (s, s)

getsEventQueue :: EventQueueCtx m => (EventQueue -> a) -> m a
getsEventQueue = flip fmap getEventQueue

putEventQueue :: EventQueueCtx m => EventQueue -> m ()
putEventQueue = stateEventQueue . const . ((,) ())

modifyEventQueue :: EventQueueCtx m => (EventQueue -> EventQueue) -> m ()
modifyEventQueue = stateEventQueue . fmap ((,) ())


-- MTL instances --

deriving instance MonadError e m => MonadError e (EventQueueT m)
deriving instance MonadReader r m => MonadReader r (EventQueueT m)
deriving instance MonadWriter w m => MonadWriter w (EventQueueT m)

instance MonadState s m => MonadState s (EventQueueT m) where
    state = lift . state


-- Common operations --

flushEventQueue :: (MonadX x m, EventQueueCtx m) => m ()
flushEventQueue = getEventQueue >>= go >>= putEventQueue
  where
    go acc = pollEvent >>= maybe (return acc) (go . (acc |>))


pollEventQueue :: (MonadX x m, EventQueueCtx m) => m (Maybe SomeEvent)
pollEventQueue = do
    flushEventQueue
    q <- getEventQueue
    case S.viewl q of
        EmptyL -> return Nothing
        e :< es -> do
            putEventQueue es
            return $ Just e


waitEventQueue :: (MonadX x m, EventQueueCtx m) => m SomeEvent
waitEventQueue = do
    flushEventQueue
    q <- getEventQueue
    case S.viewl q of
        EmptyL -> do
            waitEvent
        e :< es -> do
            putEventQueue es
            return e


partitionBy :: (a -> Maybe b) -> Seq a -> (Seq b, Seq a)
partitionBy f = foldl' part (S.empty, S.empty)
  where
    part (bs, as) a = case f a of
        Nothing -> (bs, as |> a)
        Just b -> (bs |> b, as)

takeEventsByType :: (MonadX x m, EventQueueCtx m, Event e) => m (Seq e)
takeEventsByType = flushEventQueue >> stateEventQueue (partitionBy fromEvent)

skipEventsByType :: (MonadX x m, EventQueueCtx m, Event e) => m (Maybe e)
skipEventsByType = do
    evs <- takeEventsByType
    return $ case S.viewr evs of
        EmptyR -> Nothing
        _ :> ev -> Just ev
