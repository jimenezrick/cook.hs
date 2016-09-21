module Cook.Recipe.Template (
    useTemplateWith
  ) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Text.Mustache
import Text.Mustache.Types

useTemplateWith :: ToJSON a => String -> ByteString -> a -> IO Text
useTemplateWith name tmpl conf = do
    case compileTemplate name (toStrict $ decodeUtf8 tmpl) of
        Left err    -> fail $ show err
        Right ttmpl -> return . fromStrict . substitute ttmpl $ mFromJSON conf
