#! /usr/bin/env nix-shell
#! nix-shell datahub-producer.hs.nix -i runghc

{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import Data.Typeable (Typeable)
import Data.Functor ((<&>))
import Control.Arrow (left, right)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))

import Control.Monad.Catch (Exception, MonadThrow(..))

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Aeson as J
import Data.String.Conversions (cs)
import qualified Data.Binary as BIN

import Control.Lens ((^?), (^..), folded, _Just)
import Data.Aeson.Lens (key, _Array, _String)

import qualified Data.Avro.Types as A (Value(..))
import qualified Data.Avro as A (Schema, Result(..))
import qualified Data.Avro.Schema as A (resultToEither)
import Data.Avro.JSON (decodeAvroJSON)
import Data.Avro.Encode (encodeAvro)
import Data.Avro.Decode (decodeAvro)
import Data.Avro.Deriving (makeSchema)

import Kafka.Avro (
    SchemaRegistry(..), Subject(..), SchemaId(..)
  , schemaRegistry, sendSchema
  , extractSchemaId, loadSchema
  )

import Data.Conduit (ConduitT, ZipSink(..), getZipSink, runConduitRes, runConduit, bracketP, (.|), yield)
import qualified Data.Conduit.Combinators as C
import Kafka.Conduit.Sink (ProducerRecord(..), TopicName(..), ProducePartition(..), BrokerAddress(..), kafkaSink, brokersList)

import Network.URI (parseURI)
import Network.URI.Lens (uriAuthorityLens, uriRegNameLens, uriPortLens)


data StringException = StringException String deriving (Typeable, Show)
instance Exception StringException

fromRight :: (MonadThrow m, Show a) => Either a b -> m b
fromRight = either (throwM . StringException . show) return

encodeJsonWithSchema :: (MonadIO m, MonadThrow m)
  => SchemaRegistry
  -> Subject
  -> A.Schema
  -> J.Value
  -> m B.ByteString
encodeJsonWithSchema sr subj schema json = do
  v <- fromRight $  A.resultToEither $ decodeAvroJSON schema json 
  mbSid <- fromRight =<< sendSchema sr subj schema
  return $ appendSchemaId  v mbSid
  where appendSchemaId v (SchemaId sid)= B.cons (toEnum 0) (BIN.encode sid) <> (encodeAvro v)

decodeJsonWithSchema :: (MonadIO m, MonadThrow m)
                 => SchemaRegistry
                 -> B.ByteString
                 -> m J.Value
decodeJsonWithSchema sr bs = do
  (sid, payload) <- maybe (throwM . StringException $ "BadPayloadNoSchemaId") return $ extractSchemaId bs
  schema <- fromRight =<< loadSchema sr sid
  return $ J.toJSON $ decodeAvro schema payload

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    error "please pass datahub config file"
  confJson :: J.Value <- fromRight . J.eitherDecode =<< B.readFile (head args)

  let
    topic = "MetadataChangeEvent"
    schema = $(makeSchema "../MetadataChangeEvent.avsc")
    sandboxL = key "services".key "linkedin-datahub-pipeline".key "sandbox"
    urisL = key "uris". _Array.folded._String
    brokers = confJson ^.. sandboxL.key "kafka".urisL
    -- srs = confJson ^.. sandboxL.key "schema-registry".urisL
    -- brokers' = map (\uriText -> BrokerAddress . cs . concat $ parseURI (cs uriText)  ^.. _Just.uriAuthorityLens._Just.(uriRegNameLens <> uriPortLens)) brokers

    srs = [ "http://localhost:8081" ::String ]
    brokers' = [ BrokerAddress "localhost:9092"  ]

  contents <- B.getContents <&> BC.lines
  sr <- schemaRegistry (cs (head srs))
  putStrLn "why?"
  runConduitRes $ C.yieldMany contents
               .| C.iterM (liftIO . print)
               .| C.mapM (fromRight .  J.eitherDecode)
               .| C.mapM (encodeJsonWithSchema sr (Subject (topic <> "-value")) schema)
               .| C.map (mkRecord (TopicName topic))
               .| getZipSink (ZipSink (kafkaSink (brokersList brokers')) *>
                              ZipSink ((C.length >>= yield) .| C.iterM (\n -> liftIO $ putStrLn ("total table num:" <> show n)) .| C.sinkNull))
  return ()               
  where
    mkRecord :: TopicName -> B.ByteString -> ProducerRecord
    mkRecord topic bs = ProducerRecord topic UnassignedPartition Nothing (Just (cs bs))

