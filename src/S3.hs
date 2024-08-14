#!/usr/bin/env stack
{- stack script
     --compile
     --copy-bins
     --snapshot lts-22.32
     --package amazonka
     --package amazonka-ec2
     --package amazonka-s3
     --package conduit
     --package conduit-binary
     --package generic-lens
     --package lens
     --package text
     --package bytestring
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module S3 where

import Amazonka hiding (length)
import Amazonka.S3
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as Fold
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import System.IO

getPresignedURL :: Region -> BucketName -> ObjectKey -> IO ByteString
getPresignedURL reg b k = do
  lgr <- newLogger Trace stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg
  ts <- getCurrentTime
  runResourceT $ presignURL env ts 60 (newGetObject b k)

listAll :: Region -> IO ()
listAll reg = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg

  let val :: Maybe Text -> Text
      val = fromMaybe "Nothing"

      lat :: ObjectVersion -> Text
      lat v = Text.pack . maybe mempty (mappend " - " . show) $ v ^. #isLatest

      key :: ObjectVersion -> Text
      key v =
        mconcat
          [ val (v ^? #key . traverse . _ObjectKey),
            ": ",
            val (v ^? #versionId . traverse . _ObjectVersionId),
            lat v
          ]

  runResourceT $ do
    say "Listing Buckets .."
    Just bs <- view #buckets <$> send env newListBuckets
    say $ "Found " <> Text.pack (show (length bs)) <> " Buckets."

    forM_ bs $ \(view #name -> b) -> do
      say $ "Listing Object Versions in: " <> (b ^. _BucketName)
      runConduit $
        paginate env (newListObjectVersions b)
          .| CL.concatMap (toListOf $ #versions . _Just . folded)
          .| CL.mapM_ (say . mappend " -> " . key)

getFile :: Region -> BucketName -> ObjectKey -> FilePath -> IO ()
getFile reg b k f = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg

  runResourceT $ do
    rs <- send env (newGetObject b k)
    view #body rs `sinkBody` CB.sinkFile f
    say $
      "Successfully Download: "
        <> b ^. _BucketName
        <> " - "
        <> k ^. _ObjectKey
        <> " to "
        <> Text.pack f

putChunkedFile :: Region -> BucketName -> ObjectKey -> ChunkSize -> FilePath -> IO ()
putChunkedFile reg b k c f = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg

  runResourceT $ do
    bdy <- chunkedFile c f
    void . send env $ newPutObject b k bdy
    say $
      "Successfully Uploaded: "
        <> Text.pack f
        <> " to "
        <> b ^. _BucketName
        <> " - "
        <> k ^. _ObjectKey

tagBucket :: Region -> BucketName -> [(ObjectKey, Text)] -> IO ()
tagBucket reg bkt xs = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg

  let tags = map (uncurry newTag) xs
      kv t = (t ^. #key . _ObjectKey) <> "=" <> (t ^. #value)

  runResourceT $ do
    void . send env $ newPutBucketTagging bkt (newTagging & #tagSet .~ tags)
    say $ "Successfully Put Tags: " <> Fold.foldMap kv tags

    ts <- view #tagSet <$> send env (newGetBucketTagging bkt)
    forM_ ts $ \t ->
      say $ "Found Tag: " <> kv t

getObjectAttributes :: Region -> BucketName -> ObjectKey -> IO GetObjectAttributesResponse
getObjectAttributes reg b k = do
  lgr <- newLogger Trace stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg
  let req =
        newGetObjectAttributes b k
          & #objectAttributes
            .~ [ ObjectAttributes_ETag,
                 ObjectAttributes_ObjectParts,
                 ObjectAttributes_StorageClass
               ]
  runResourceT $ send env req

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLnw
