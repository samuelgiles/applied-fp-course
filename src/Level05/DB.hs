{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


module Level05.DB
  ( FirstAppDB (FirstAppDB),
    initDB,
    closeDB,
    addCommentToTopic,
    getComments,
    getTopics,
    deleteTopic,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam(..))
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.SQLite.Simple
  ( Connection,
    Query (fromQuery),
  )
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.SimpleErrors as Sql
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import Level05.AppM (AppM, liftEither)
import Level05.DB.Types (DBComment)
import Level05.Types.Topic (getTopicAsText)
import Level05.Types.CommentText (getCommentTextAsText)
import Level05.Types
  ( Comment,
    CommentText,
    Error (DBError),
    Topic,
    fromDBComment,
    getCommentText,
    getTopic,
    mkTopic,
  )

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB
  = FirstAppDB
      { dbConn :: Connection
      }

-- Quick helper to pull the connection and close it down.
closeDB ::
  FirstAppDB ->
  IO ()
closeDB =
  Sql.close . dbConn

initDB ::
  FilePath ->
  IO (Either SQLiteResponse FirstAppDB)
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
    -- Query has an `IsString` instance so string literals like this can be
    -- converted into a `Query` type when the `OverloadedStrings` language
    -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB ::
  forall a b .
  (a -> Either Error b) ->
  IO a ->
  AppM b
runDB feeb ioa =
  liftIO (first DBError <$> Sql.runDBAction ioa) >>= liftEither >>= liftEither . feeb


-- Move your use of DB.runDBAction to this function to avoid repeating
-- yourself in the various DB functions.

getComments ::
  FirstAppDB ->
  Topic ->
  AppM [Comment]
getComments FirstAppDB {dbConn} topic =
  runDB (traverse fromDBComment) query
  where
    query :: IO [DBComment]
    query = Sql.queryNamed dbConn sql [":topic" := getTopicAsText topic]
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = :topic"


addCommentToTopic ::
  FirstAppDB ->
  Topic ->
  CommentText ->
  AppM ()
addCommentToTopic FirstAppDB {dbConn} topic commentText = do
  currentTime <- liftIO getCurrentTime
  let query = Sql.executeNamed dbConn sql [":topic" := getTopicAsText topic, ":comment" := getCommentTextAsText commentText, ":time" := currentTime]
  runDB pure query
  where
    sql = "INSERT INTO comments (topic,comment,time) VALUES (:topic,:comment,:time)"

getTopics ::
  FirstAppDB ->
  AppM [Topic]
getTopics FirstAppDB {dbConn} =
  runDB pure query
  where
    query :: IO [Topic]
    query = Sql.query_ dbConn sql
    sql = "SELECT DISTINCT topic FROM comments"

deleteTopic ::
  FirstAppDB ->
  Topic ->
  AppM ()
deleteTopic FirstAppDB {dbConn} topic =
  runDB pure query
  where
    sql = "DELETE FROM comments WHERE topic = :topic"
    query = Sql.executeNamed dbConn sql [":topic" := getTopicAsText topic]

-- Go to 'src/Level05/Core.hs' next.
