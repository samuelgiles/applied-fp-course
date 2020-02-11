module Level05.Types.Topic
  ( Topic,
    mkTopic,
    getTopic,
    encodeTopic,
    getTopicAsText,
  )
where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Level05.Types.Error (Error (EmptyTopic), nonEmptyText)
import Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

newtype Topic = Topic {
  getTopicAsText :: Text
}
  deriving (Show)

encodeTopic :: Applicative f => Encoder f Topic
encodeTopic = getTopic >$< E.text

mkTopic ::
  Text ->
  Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic ::
  Topic ->
  Text
getTopic (Topic t) =
  t

instance FromRow Topic where
  fromRow = Topic <$> field
