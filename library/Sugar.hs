module Sugar where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Name = Text
type Var = Text
type Field = Text

data Type = Type
    { tName :: Name
    , tParam :: [Var]
    } deriving (Show, Eq)

data Alias = Alias
    { aName :: Type
    , aDefinition :: Type
    } deriving (Show, Eq)

data Record = Record
    { rType :: Type
    , rFields :: Map Field Type
    } deriving (Show, Eq)

data Variant = Variant
    { vType :: Type
    , vCases :: [Case]
    } deriving (Show, Eq)

data Case
    = Case'Ref Type
    | Case'Record Record
    | Case'Variant Variant
    | Case'Tag Name
    deriving (Show, Eq)

data Decl
    = Decl'Alias Alias
    | Decl'Record Record
    | Decl'Variant Variant
    deriving (Show, Eq)

type File = [Decl]

-- Writer

typeW :: Type -> Text
typeW Type{tName,tParam} = tName <> T.concat (map (\p -> " " <> p) tParam)

aliasW :: Alias -> Text
aliasW Alias{aName,aDefinition} = typeW aName <> " = " <> typeW aDefinition

-- Parser

type Parser = Parsec Void Text

commentsP :: Parser ()
commentsP = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"
