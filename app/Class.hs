module Class where

import Control.Arrow
import Protolude (note, Either, pure, ($), (<&>), (&))
import qualified Data.Text.Lazy as LT
import Data.Map.Strict ( (!?), Map )
import Types

class ReadBody a where
  readBody :: Map LT.Text LT.Text -> Either LT.Text a

instance ReadBody RegisterUser where
  readBody params = do
    usr <- params !? "username" & note "Missing Username" <&> (LT.toStrict >>> Username)
    pass <- params !? "password" & note "Missing Password" <&> (LT.toStrict >>> RawPassword)
    pure $ RegisterUser usr pass