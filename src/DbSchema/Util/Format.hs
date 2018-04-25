module DbSchema.Util.Format(format, TF.Only(..)) where

import qualified Data.Text               as T
import qualified Data.Text.Format        as TF
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy          as TL

format :: Params ps => TF.Format -> ps -> T.Text
format s = TL.toStrict . TF.format s
