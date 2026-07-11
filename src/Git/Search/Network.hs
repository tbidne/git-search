{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Git.Search.Network
  ( -- * Effect
    Network,
    runNetwork,

    -- * Functions
    newTlsManager,
    mkJsonRequest,
    runJsonRequest,

    -- * Exceptions
  )
where

import Data.Aeson qualified as Asn
import Effectful (Dispatch (Static), DispatchOf, Effect)
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
import Git.Search.Prelude
import Network.HTTP.Client (BodyReader, Manager, Request, Response)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as TLS
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status

data Network :: Effect

type instance DispatchOf Network = Static WithSideEffects

data instance StaticRep Network = MkNetwork

runNetwork :: (HasCallStack, IOE :> es) => Eff (Network : es) a -> Eff es a
runNetwork = evalStaticRep MkNetwork

newTlsManager :: (HasCallStack, Network :> es) => Eff es Manager
newTlsManager = unsafeEff_ TLS.newTlsManager

mkJsonRequest :: (HasCallStack, Network :> es) => Maybe OsString -> String -> Eff es Request
mkJsonRequest mAuth url = unsafeEff_ $ do
  baseReq <- mkReq

  authHeader <- case mAuth of
    Nothing -> pure []
    Just auth -> do
      authBs <- encodeUtf8 . pack <$> decodeThrowM auth
      pure [("Authorization", "Bearer " <> authBs)]

  pure $ updateReq authHeader baseReq
  where
    mkReq = HttpClient.parseRequest url
    updateReq auth r =
      r
        { HttpClient.requestHeaders =
            [ ("Accept", "application/json;charset=utf-8,application/json"),
              -- Need this agent so github does not block us.
              ("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
            ]
              ++ auth
        }

runJsonRequest ::
  forall a es.
  ( FromJSON a,
    HasCallStack,
    Network :> es
  ) =>
  String ->
  Manager ->
  Request ->
  Eff es a
runJsonRequest url manager req =
  unsafeEff_
    $ HttpClient.withResponse req manager readResponse
  where
    readResponse :: Response BodyReader -> IO a
    readResponse res = do
      let bodyReader = HttpClient.responseBody res
          status = HttpClient.responseStatus res
          statusCode = getStatusCode res
          mkEx rsn = MkNetworkException rsn url

      when (statusCode /= 200)
        $ throwIO
        $ mkEx (ReasonStatus status)

      bodyBs <-
        mapThrowLeft
          (mkEx . ReasonReadBody)
          =<< trySync (mconcat <$> HttpClient.brConsume bodyReader)

      mapThrowLeft
        (mkEx . ReasonDecodeJson bodyBs)
        (Asn.eitherDecodeStrict bodyBs)

-- | Exception reason.
data ExceptionReason
  = -- | Received non-200.
    ReasonStatus Status
  | -- | Exception when reading the body.
    ReasonReadBody SomeException
  | -- | Exception decoding JSON. The first string is the json we attempted
    -- to decode. The second is the error message.
    ReasonDecodeJson ByteString String
  | -- | Exception decoding JSON. The first string is the bytestring we
    -- attempted to decode. The second is the error message.
    ReasonDecodeUtf8 ByteString UnicodeException
  deriving stock (Show)

-- | General network exception.
data NetworkException = MkNetworkException
  { reason :: ExceptionReason,
    url :: String
  }
  deriving stock (Show)

instance Exception NetworkException where
  displayException ex =
    case ex.reason of
      ReasonStatus status ->
        if is404 status
          then
            mconcat
              [ "Received 404 for url '",
                url,
                "': ",
                statusMessage status
              ]
          else
            mconcat
              [ "Received ",
                show $ Status.statusCode status,
                " for url '",
                url,
                "': ",
                statusMessage status
              ]
      ReasonReadBody readBodyEx ->
        mconcat
          [ "Exception reading body for url '",
            url,
            "':\n\n",
            displayException readBodyEx
          ]
      ReasonDecodeJson jsonBs err ->
        mconcat
          [ "Could not decode JSON: ",
            err,
            ". Bytes: ",
            show jsonBs
          ]
      ReasonDecodeUtf8 bs err ->
        mconcat
          [ "Could not decode UTF-8: ",
            displayException err,
            ". Bytes: ",
            show bs
          ]
    where
      url = ex.url
      is404 x = Status.statusCode x == 404

      statusMessage s =
        mconcat
          [ "Status message: ",
            show $ Status.statusMessage s
          ]

getStatusCode :: Response body -> Int
getStatusCode = Status.statusCode . HttpClient.responseStatus
