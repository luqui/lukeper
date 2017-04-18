{-# LANGUAGE ExistentialQuantification #-}

module Signals where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad (ap)

-- There are a few different kinds of signal:
--   Piecewise: a "stepper", that takes on a series of values, each for an interval of time
--   Event: a series of time-stamped values ; a finite sum of "delta functions"
--   Continuous: continuously changing with time
--   Track:  an optimization of Continuous Double, working with Array windows

-- Each of these types has a different has a way an SF depends both covariantly and contravariantly.  
-- Let's look at some examples.  The arrows are to be taken as stateful.
--
--    Piecewise a ~> Piecewise b = t -> a -> (t,b)   -- we need the incoming t because it might affect how the state is updated
--    Event a     ~> Event b     = t -> (a -> () , Maybe (t,b))
--    Sample a    ~> Sample b    = t -> a -> b
--    Track ~> Track = Buffer a -> MBuffer s b -> ST s ()
--
--    Event a   ~>   Piecewise b = t -> ((t,b) , a -> (t,b))
--                                       ^^^^    ^^^^^^^^^^
--                            no incoming event   incoming event
--
--    Piecewise a ~> Event b = a -> t -> Maybe (t,b)

-- There are some patterns here. 
--    ? -> Piecewise -- Needs to return the window, the (possible) beginning of the next piece
--    Piecewise -> ? -- Needs given window size, if only to update the state appropriately.
--    ? -> Event     -- Needs a given window, and returns Maybe (t,b), because the event may or may not occur
--    Event -> ?     -- Two alternatives: an event occurred in the window, or it did not.
--
-- Seems like a given window is necessary no matter what, except for Continuous
-- (one could say that the t in t -> b is the window, we just immediately
-- sample at the end of the window).  Continuous is strange when the arrows are
-- stateful, actually -- I guess instead of Continuous it's more like Sample,
-- where we trade one input sample for one output sample.  And now we still
-- need a window -- how long is a sample?

-- Assuming state and given window, we have:
--
--   (Piecewise a ~>)   = (a ->)
--   (Event a ~>)       = (a ->) x Id   = (Maybe a ->)
--   (~> Piecewise a)   = (-> (t, a))
--   (~> Event a)       = (-> Maybe (t, a))
--
-- There appears to be a symmetry surfacing.  The reason it is not perfect is
-- because of differences in calling convention.  For example, (Event a ~>) is
-- not (Maybe (t, a) ->) because we assume that the function is called at transitions,
-- so we know that if there is an event, its incoming time will be 0.
--
-- Perhaps we can make the same assumption on the other end, and assume that if
-- an event occurs, it occurs at the end of the window.  And so we can use this
-- as a base abstraction:

newtype Window t a = Window { runWindow :: t -> (t, a) }

-- In which we are given a maximum time window, and we return the actual length
-- of the window together with a value.  There is a monad on Window, in which
-- binding runs the first action, then the second action with the remaining
-- time (if any). 

instance Functor (Window t) where
    fmap f (Window w) = Window ((fmap.fmap) f w)

instance (Num t) => Applicative (Window t) where
    pure = return
    (<*>) = ap

instance (Num t) => Monad (Window t) where
    return x = Window (\t -> (0, x))   -- NB this is *not* a state monad
    m >>= f = Window $ \t ->
        let (t', x) = runWindow m t in
        first (t' +) (runWindow (f x) (t-t'))

--
-- Taking (->) to mean Kliesli (Window t), we get
--
--   (Piecewise a ~>) = (a ->)
--   (Event a ~>)     = (Maybe a ->)
--   (~> Piecewise a) = (-> a)
--   (~> Event a)     = (-> Maybe a)
--
-- Which shows us that Piecewise = a and Event = Maybe a, at least
-- representationally.  There are implicit differences, for example, two events
-- can not come at different times without a Nothing in between.

-- Then to represent local states, we can use an arrow.

data Stateful p a b = forall s. Stateful (p (s,a) (s,b))

instance Arrow p => Category (Stateful p) where
    id = Stateful id
    -- f :: p (s,a) (s,b)
    -- g :: p (s',b) (s',c)
    -- _ :: p ((s,s'),a) ((s,s'),c)
    --
    -- ((s,s'),a) ----> (s',(s,a)) --> (s',(s,b)) ----> (s,(s',b)) ----> (s,(s',c)) -----> ((s,s'),c)
    --            shuf1          second f         shuf2           second g          unshuf
    Stateful g . Stateful f = Stateful (arr shuf1 >>> second f >>> arr shuf2 >>> second g >>> arr unshuf)
        where
        shuf1 ((s,s'),a) = (s',(s,a))
        shuf2 (s',(s,b)) = (s,(s',b))
        unshuf (s,(s',c)) = ((s,s'),c)

instance Arrow p => Arrow (Stateful p) where
    arr f = Stateful (arr (second f))
    -- f :: p (s,b) (s,c)
    -- g :: p (s',b') (s',c')
    -- _ :: p ((s,s'),(b,b')) ((s,s'),(c,c'))
    --
    -- ((s,s'),(b,b')) --> ((s,b),(s',b')) --> ((s,c),(s',c')) --> ((s,s'),(c,c'))
    --                shuf               f *** g              unshuf
    Stateful f *** Stateful g = Stateful (arr shuf >>> (f *** g) >>> arr unshuf)
        where
        shuf ((s,s'),(b,b')) = ((s,b),(s',b'))
        unshuf ((s,c),(s',c')) = ((s,s'),(c,c'))

instance ArrowChoice p => ArrowChoice (Stateful p) where
    -- f :: p (s,b) (s,c)
    -- g :: p (s',b') (s',c')
    -- _ :: p ((s,s'), Either b b') ((s,s'), Either c c')
    --
    -- ((s,s'), Either b b') ----> Either (s',(s,b)) (s,(s',b')) --> Either (s',(s,c)) (s,(s',c')) --> ((s,s'), Either c c')
    --                       shuf                       second f +++ second g                    unshuf
    Stateful f +++ Stateful g = Stateful (arr shuf >>> (second f +++ second g) >>> arr unshuf)
        where
        shuf ((s,s'), Left b) = Left (s',(s,b))
        shuf ((s,s'), Right b) = Right (s,(s',b))
        unshuf (Left (s',(s,c))) = ((s,s'), Left c)
        unshuf (Right (s,(s',c))) = ((s,s'), Right c)

instance ArrowLoop p => ArrowLoop (Stateful p) where
    -- f :: p (s, (b, d)) (s, (c, d))
    -- _ :: p (s, b) (s, c)
    --
    -- ((s,b),d) --> (s, (b,d)) --> (s, (c,d)) --> ((s,c),d)
    --          shuf             f            unshuf
    loop (Stateful f) = Stateful (loop (arr shuf >>> f >>> arr unshuf))
        where
        shuf ((s,b),d) = (s,(b,d))
        unshuf (s,(c,d)) = ((s,c),d)
