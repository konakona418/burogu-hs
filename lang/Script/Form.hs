{- | The surface syntax of the script language: S-expression forms as
read from source, before evaluation. Every form carries the source
position where it started, so runtime and macro errors can point back
at the offending code.
-}
module Script.Form (Form (..), Node (..), Pos) where

import Data.Scientific (Scientific)
import Data.Text (Text)

-- | A 1-based (line, column) source position.
type Pos = (Int, Int)

{- | One form. The position is the location of the first character of
the form (the opening delimiter or the first atom character). Forms
built by macros have position (0, 0).
-}
data Form = Form
    { formPos :: Pos
    , formNode :: Node
    }
    deriving (Show, Eq)

data Node
    = NNum Scientific
    | NStr Text
    | NBool Bool
    | NNil
    | NSym Text
    | NList [Form]
    | NArray [Form]
    | NMap [(Form, Form)]
    deriving (Show, Eq)
