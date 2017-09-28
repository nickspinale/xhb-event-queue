# xhb-event-queue

This package provides a monad transformer that allows for operations on the
[xhb](https://hackage.haskell.org/package/xhb) event queue,
along with some useful such cases:

```haskell
class Monad m => EventQueueCtx m where
    stateEventQueue :: (EventQueue -> (a, EventQueue)) -> m a
```

## Example

C program reacting to a `MotionNotify` event might use something like the following to only use the most recent `MotionNotify` event (because ones from the past are useless and can cause lag):

```c
while(XCheckTypedEvent(dpy, MotionNotify, &ev));
```

This package contains the following function:

```haskell
skipEventsByType :: (MonadX x m, EventQueueCtx m, Event e) => m (Maybe e)
```

which can be used to the effect of `XCheckTypedEvent` like so:

```haskell
onMotionNotify :: (MonadX IO m, EventQueueCtx m) => MotionNotifyEvent -> m ()
onMotionNotify ev = do
    flushEventQueue -- move events from xhb channel to our state
    MkMotionNotifyEvent{..} <- fromMaybe ev <$> skipEventsByType
    ...
```

## Documentation

[This article](http://nickspinale.com/articles/xhb-monad) describes this package and some of its friends in detail.

Haddock can be found [here](https://nspin.github.io/xhb-event-queue).
