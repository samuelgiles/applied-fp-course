{-# LANGUAGE OverloadedStrings #-}

module Level02.Core
  ( runApp,
    app,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.Either (either)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Level02.Types
  ( ContentType(..),
    Error(..),
    RqType(..),
    mkCommentText,
    mkTopic,
    renderContentType,
    getTopic
  )
import Network.HTTP.Types
  ( Status,
    Header,
    hContentType,
    status200,
    status400,
    status404,
  )
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    requestMethod,
    responseLBS,
    strictRequestBody,
  )
import Network.Wai.Handler.Warp (run)

-- | -------------------------------------------|
--  |- Don't start here, go to Level02.Types!  -|
--  |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse ::
  Status ->
  ContentType ->
  LBS.ByteString ->
  Response
mkResponse status contentType s =
  let
    contentTypeHeader :: Header
    contentTypeHeader = ("Content-Type", renderContentType contentType)
  in
    responseLBS status [contentTypeHeader] s


resp200 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp200 =
  mkResponse status200

resp404 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp404 =
  mkResponse status404

resp400 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp400 =
  mkResponse status400

-- | ----------------------------------------------------------------------------------
--  These next few functions will take raw request information and construct         --
--  one of our types.                                                                --
--                                                                                   --
--  By breaking out these smaller functions, we're able to isolate our               --
--  validation requirements into smaller components that are simpler to maintain     --
--  and verify. It also allows for greater reuse and it also means that              --
--  validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------
mkAddRequest ::
  Text ->
  LBS.ByteString ->
  Either Error RqType
mkAddRequest txt lbs =
  let
    eitherTopic = mkTopic txt
    eitherCommentText = mkCommentText $ lazyByteStringToStrictText lbs
  in
    case (eitherTopic, eitherCommentText) of
      (Right topic, Right commentText) ->
        Right $ AddRq topic commentText
      (Left topicError, _) ->
        Left topicError
      (_, Left commentError) ->
        Left commentError
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest ::
  Text ->
  Either Error RqType
mkViewRequest topicName =
  let
    eitherTopic = mkTopic topicName
  in
    case eitherTopic of
      Left _ ->
        Left MissingTopicError
      Right topic ->
        Right $ ViewRq topic

mkListRequest ::
  Either Error RqType
mkListRequest =
  Right ListRq

-- | ----------------------------------
--  end of RqType creation functions --
--------------------------------------
mkErrorResponse ::
  Error ->
  Response
mkErrorResponse e =
  case e of
    MissingTopicError ->
      resp404 ContentTypeText "Missing topic"
    EmptyTopicError ->
      resp400 ContentTypeText "Empty topic"
    EmptyCommentError ->
      resp400 ContentTypeText "Empty Comment Text"
    RoutingError ->
      resp404 ContentTypeText "Unknown route"


-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest ::
  Request ->
  IO (Either Error RqType)
mkRequest request = do
  body <- strictRequestBody request
  let path = pathInfo request
  return $ case path of
    [topicName, "add"] ->
      mkAddRequest topicName body
    [topicName, "view"] ->
      mkViewRequest topicName
    ["list"] ->
      mkListRequest
    _ ->
      Left RoutingError


-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest ::
  RqType ->
  Either Error Response
handleRequest rqType =
  case rqType of
    AddRq _topic _commentText ->
      Right $ resp200 ContentTypeText "AddRq not implemented yet"
    ViewRq _topic ->
      Right $ resp200 ContentTypeText "ViewRq not implemented yet"
    ListRq ->
      Right $ resp200 ContentTypeText "ListRq not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app ::
  Application
app request respond = do
  eReqType <- mkRequest request
  case eReqType of
    Right reqType ->
      case handleRequest reqType of
        Right processedRequest ->
          respond processedRequest
        Left err ->
          respond $ mkErrorResponse err
    Left err ->
      respond $ mkErrorResponse err

runApp :: IO ()
runApp = run 3000 app
