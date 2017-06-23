{-# LANGUAGE DeriveFunctor, ExistentialQuantification, TupleSections #-}

module Signals where

import Prelude hiding (id, (.))
import Control.Monad (ap)

import Control.Arrow
import Control.Category
import Control.Monad.Trans.Class

newtype WindowT t m a = WindowT { runWindowT :: (t, t) -> m (t, a) }

-- In which we are given a potential time window as context, and we return the
-- actual right endpoint.  The monad combines actions by returning the smallest
-- window.

instance (Functor m) => Functor (WindowT t m) where
    fmap f (WindowT w) = WindowT ((fmap.fmap.fmap) f w)

instance (Ord t, Monad m) => Applicative (WindowT t m) where
    pure = return
    (<*>) = ap

instance (Ord t, Monad m) => Monad (WindowT t m) where
    return x = WindowT (\(_, tmax) -> return (tmax, x)) -- return the largest valid window
    m >>= f = WindowT $ \(tmin, tmax) -> do
        (t', x) <- runWindowT m (tmin, tmax)
        (t'', y) <- runWindowT (f x) (tmin, t')
        return (t'', y)

instance MonadTrans (WindowT t) where
    lift m = WindowT (\(_, tmax) -> (tmax,) <$> m)

window :: (Monad m) => WindowT t m (t,t)
window = WindowT (\(tmin, tmax) -> return (tmax, (tmin, tmax)))

restrict :: (Monad m, Ord t) => t -> WindowT t m ()
restrict newtmax = WindowT (\(tmin, tmax) -> return (max tmin (min tmax newtmax), ()))

instant :: (Monad m) => a -> WindowT t m a
instant x = WindowT (\(tmin, _) -> return (tmin, x))

-- With this monad instance, everything is uniform as long as events occur at
-- the *beginning* of their window.


data Stateful p a b = forall s. Stateful s (p (s,a) (s,b))

instance Arrow p => Category (Stateful p) where
    id = Stateful () id
    -- f :: p (s,a) (s,b)
    -- g :: p (s',b) (s',c)
    -- _ :: p ((s,s'),a) ((s,s'),c)
    --
    -- ((s,s'),a) ----> (s',(s,a)) --> (s',(s,b)) ----> (s,(s',b)) ----> (s,(s',c)) -----> ((s,s'),c)
    --            shuf1          second f         shuf2           second g          unshuf
    Stateful s0' g . Stateful s0 f = Stateful (s0,s0') (arr shuf1 >>> second f >>> arr shuf2 >>> second g >>> arr unshuf)
        where
        shuf1 ((s,s'),a) = (s',(s,a))
        shuf2 (s',(s,b)) = (s,(s',b))
        unshuf (s,(s',c)) = ((s,s'),c)

instance Arrow p => Arrow (Stateful p) where
    arr f = Stateful () (arr (second f))
    -- f :: p (s,b) (s,c)
    -- g :: p (s',b') (s',c')
    -- _ :: p ((s,s'),(b,b')) ((s,s'),(c,c'))
    --
    -- ((s,s'),(b,b')) --> ((s,b),(s',b')) --> ((s,c),(s',c')) --> ((s,s'),(c,c'))
    --                shuf               f *** g              unshuf
    Stateful s0 f *** Stateful s0' g = Stateful (s0,s0') (arr shuf >>> (f *** g) >>> arr unshuf)
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
    Stateful s0 f +++ Stateful s0' g = Stateful (s0,s0') (arr shuf >>> (second f +++ second g) >>> arr unshuf)
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
    loop (Stateful s0 f) = Stateful s0 (loop (arr shuf >>> f >>> arr unshuf))
        where
        shuf ((s,b),d) = (s,(b,d))
        unshuf (s,(c,d)) = ((s,c),d)


-- With the Window monad and Stateful arrows at our disposal, we can now define
-- our signal transform type.

type SigTrans t m = Stateful (Kleisli (WindowT t m))

stateProc :: s -> (s -> a -> WindowT t m (s,b)) -> SigTrans t m a b
stateProc s0 = Stateful s0 . Kleisli . uncurry

newtype Piecewise a = Piecewise { getPiecewise :: a }
    deriving (Functor)
newtype Event a = Event { getEvent :: Maybe a }
    deriving (Functor)

stepper :: (Ord t, Monad m) => a -> SigTrans t m (Event a) (Piecewise a)
stepper x0 = Stateful x0 . arr $ \(xcur,e) -> 
    case getEvent e of
        Nothing -> (xcur, Piecewise xcur)
        Just xnext -> (xnext, Piecewise xnext)

countEvents :: (Ord t, Monad m) => SigTrans t m (Event a) (Event (Int,a))
countEvents = Stateful 0 . arr $ \(count,e) ->
    count `seq` case getEvent e of
        Nothing -> (count, Event Nothing)
        Just x  -> (count+1, Event (Just (count,x)))

