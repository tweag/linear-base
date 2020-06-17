{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module Linear where

import qualified Control.Monad.Linear as Linear
import Streaming.Linear (Stream, Of)
import qualified Streaming.Linear as S
import System.IO hiding (readLn)
import qualified System.IO.Resource as Linear
import Data.Unrestricted.Linear
import qualified Prelude.Linear as Linear
import qualified Control.Monad.Linear.Builder as Control
import qualified Data.Text as Text
import qualified Unsafe.Linear as Unsafe


streamTwoLines :: Stream (Of Text.Text) Linear.RIO ()
streamTwoLines = do
    -- open up a file handle
    handle <- Linear.lift Linear.$ Linear.openFile "temp.txt" ReadMode
    -- Stream two lines
    (Unrestricted text, handle') <- Linear.lift Linear.$ Linear.hGetLine handle
    S.yield text
    (Unrestricted text', handle'') <- Linear.lift Linear.$ Linear.hGetLine handle'
    S.yield text'
    -- close the file handle again
    Linear.lift Linear.$ Linear.hClose handle''
    where
      Control.Builder {..} = Control.monadBuilder


-- | This is forced to be safe and *not* a bad read!
-- note that the uses of unsafeFromSystemIO are safe
badRead :: Stream (Of Text.Text) Linear.RIO () #-> Linear.RIO (Unrestricted ())
badRead s = do
  nextVal <- (S.next s)
  Unrestricted () <- withNext nextVal
  return (Unrestricted ())
  where
    Control.Builder {..} = Control.monadBuilder

withNext ::
  Linear.Either () (Text.Text, Stream (Of Text.Text) Linear.RIO ()) #-> 
    Linear.RIO (Unrestricted ())
withNext (Linear.Right (v1, s))  =  putStrRIO v1 Linear.>> badRead s
withNext (Linear.Left ()) = Linear.return (Unrestricted ())


putStrRIO :: Text.Text #-> Linear.RIO ()
putStrRIO = Unsafe.toLinear putStrRIO'

putStrRIO' :: Text.Text -> Linear.RIO ()
putStrRIO' text = do
  handle <- Linear.hPutStrLn Linear.stdout text
  return (unsafeConsume handle)
  where
    unsafeConsume :: a #-> ()
    unsafeConsume = Unsafe.toLinear (\_ -> ())
    Control.Builder {..} = Control.monadBuilder





