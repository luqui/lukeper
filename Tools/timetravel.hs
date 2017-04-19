{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances #-}

import Data.Monoid
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad (ap)

class (Monoid w) => Invertible w where
    inverse :: w -> w

data IWriter r w a = IWriter { unIWriter :: ContT r (Writer w) a }

instance Functor (IWriter r w) where
    fmap f (IWriter m) = IWriter (fmap f m)

instance Applicative (IWriter r w) where
    pure = return
    (<*>) = ap

instance Monad (IWriter r w) where
    return = IWriter . return
    IWriter m >>= f = IWriter (m >>= unIWriter . f)

abort :: (Monoid w) => r -> IWriter r w a
abort r = IWriter . ContT $ \_ -> return r

capture :: (Invertible w) => a -> IWriter r w (a, a -> IWriter r w b)
capture x = IWriter . ContT $ \c -> -- c :: (a, a -> IWriter r w b) -> Writer w r
    let changeMind origx newx = 
            -- Re-run from the checkpoint, aborting at the "changeMind" call
            -- so we can figure out all the actions we ran.
            let (_, since) = runWriter (c (origx, \_ -> abort undefined))
            -- Invert all those actions, and re-run from the checkpoint with the new value.
            in IWriter . ContT $ \_ -> tell (inverse since) >> c (newx, changeMind newx)
    in c (x, changeMind x)

tellI :: (Monoid w) => w -> IWriter r w ()
tellI = IWriter . lift . tell

data InvE a = F a | I a
    deriving (Show)

invertE :: InvE a -> InvE a
invertE (F a) = I a
invertE (I a) = F a

instance Invertible [InvE a] where
    inverse = reverse . map invertE
    
runIWriter :: (Monoid w) => IWriter r w r -> (r, w)
runIWriter m = runWriter (runContT (unIWriter m) return)

iwTest :: (Show r, Show a) => IWriter r [a] r -> IO ()
iwTest m = do
    let (x, log) = runIWriter m
    mapM_ print log
    print x


test = iwTest $ do
    tellI [F "Start"]
    (n, change) <- capture 1
    tellI [F $ "Number is " ++ show n]
    dec (change (2*n)) 10 n
    where
    dec retry 0 n = tellI [F $ "We are done"] >> return n
    dec retry m 0 = tellI [F $ "Number is too small, retrying"] >> retry
    dec retry m n = do
        tellI [ F $ "Decrementing " ++ show n ++ " --> " ++ show (n-1) ]
        dec retry (m-1) (n-1)
