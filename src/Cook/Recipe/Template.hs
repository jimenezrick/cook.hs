module Cook.Recipe.Template (
    useTemplateWith
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text
import Data.Text.Encoding
import Text.Mustache
import Text.Mustache.Types

useTemplateWith :: ToJSON a => String -> ByteString -> a -> IO Text
useTemplateWith name tmpl conf =
    case compileTemplate name (decodeUtf8 tmpl) of
        Left err    -> fail $ show err
        Right ttmpl -> return . substitute ttmpl $ mFromJSON conf
