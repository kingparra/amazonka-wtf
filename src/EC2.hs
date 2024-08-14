#!/usr/bin/env stack
{- stack script
     --compile
     --copy-bins
     --snapshot lts-22.32
     --package amazonka
     --package amazonka-ec2
     --package conduit
     --package generic-lens
     --package lens
     --package bytestring
-}


{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Amazonka
import Amazonka.EC2
-- Lens is a data traversal library like JMESPath, but not limited to JSON.
import Control.Lens
import Control.Monad.IO.Class
-- Why use ByteString rather than String?
import Data.ByteString.Builder (hPutBuilder)
-- Conduit is used to work with paginated json output without loading everything into memory at once.
import Data.Conduit
import qualified Data.Conduit.List as CL
-- This blank import brings instance declarations of IsLabel into scope, which is used to resolve labels.
import Data.Generics.Labels ()
import System.IO


-- Why are we building a bytestring instead of using String?
pp x = mconcat
  [ "[instance: "       <> build (x ^. #instanceId) <> "] {",
    "\n  public-dns = " <> build (x ^. #publicDnsName),
    "\n  tags       = " <> build (x ^. #tags . to show),
    "\n  state      = " <> build (x ^. #state . #name . to fromInstanceStateName),
    "\n}\n"
  ]


instanceOverview :: IO ()
instanceOverview = do
  env <- newEnv discover
  -- What does runRestorceT do?
  runResourceT . runConduit $
    paginate env newDescribeInstances
      .| CL.concatMap (view $ #reservations . _Just)
      .| CL.concatMap (view $ #instances . _Just)
      .| CL.mapM_ (liftIO . hPutBuilder stdout . pp)


main = instanceOverview
