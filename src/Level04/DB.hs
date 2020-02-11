{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Level04.DB
  ( FirstAppDB (FirstAppDB),
    initDB,
    closeDB,
    addCommentToTopic,
    getComments,
    getTopics,
    deleteTopic,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Connection, close, Query (Query))
import qualified Database.SQLite.Simple as Sql
import Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.SimpleErrors as Sql
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import Level04.DB.Types (DBComment)
import Level04.Types.Topic (getTopicAsText, mkTopic)
import Level04.Types.CommentText (getCommentTextAsText)
import Level04.Types
  ( Comment,
    CommentText,
    Error(..),
    Topic,
    fromDBComment,
  )

-- ------------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple & sqlite-simple-errors handy! |
-- ------------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB
  = FirstAppDB
      { dbConn :: Connection
      }

-- Quick helper to pull the connection and close it down.
closeDB ::
  FirstAppDB ->
  IO ()
closeDB app =
  close $ dbConn app

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB ::
  FilePath ->
  IO (Either SQLiteResponse FirstAppDB)
initDB fp = do
  conn <- Sql.open (show fp)
  res <- Sql.runDBAction (Sql.execute_ conn createTableQ)

  pure $ (const $ FirstAppDB { dbConn = conn }) <$> res

  where
    -- Query has an `IsString` instance so string literals like this can be
    -- converted into a `Query` type when the `OverloadedStrings` language
    -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments ::
  FirstAppDB ->
  Topic ->
  IO (Either Error [Comment])
getComments app topic = do
  res <- Sql.runDBAction query
  case res of
    Left a ->
      pure $ Left DBError
    Right dbComments ->
      pure $ sequence (fromDBComment <$> dbComments)
  where
    query :: IO [DBComment]
    query = Sql.queryNamed conn sql [":topic" := getTopicAsText topic]
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = :topic"
    conn = dbConn app



addCommentToTopic ::
  FirstAppDB ->
  Topic ->
  CommentText ->
  IO (Either Error ())
addCommentToTopic app topic commentText = do
  currentTime <- getCurrentTime
  let query = Sql.executeNamed conn sql [":topic" := getTopicAsText topic, ":comment" := getCommentTextAsText commentText, ":time" := currentTime]
  res <- Sql.runDBAction query

  case res of
    Left a ->
      pure $ Left DBError
    Right _ ->
      pure $ Right ()

  where
    conn = dbConn app
    sql = "INSERT INTO comments (topic,comment,time) VALUES (:topic,:comment,:time)"

getTopics ::
  FirstAppDB ->
  IO (Either Error [Topic])
getTopics app = do
  res <- Sql.runDBAction query
  case res of
    Left a ->
      pure $ Left DBError
    Right topicsAsText ->
      pure $ Right topicsAsText
  where
    query :: IO [Topic]
    query = Sql.query_ conn sql
    sql = "SELECT DISTINCT topic FROM comments"
    conn = dbConn app

deleteTopic ::
  FirstAppDB ->
  Topic ->
  IO (Either Error ())
deleteTopic app topic = do
  let query = Sql.executeNamed conn sql [":topic" := getTopicAsText topic]
  res <- Sql.runDBAction query

  case res of
    Left a ->
      pure $ Left DBError
    Right _ ->
      pure $ Right ()

  where
    conn = dbConn app
    sql = "DELETE FROM comments WHERE topic = :topic"
