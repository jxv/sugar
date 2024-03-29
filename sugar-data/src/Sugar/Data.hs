{-# LANGUAGE OverloadedStrings #-}
module Sugar.Data
  ( Type(..)
  , TypeRef(..)
  , Ty(..)
  , Rec(..)
  , Mem(..)
  , Adt(..)
  , Cons(..)
  , ConsValue(..)
  , Err(..)
  , Error(..)
  , ParamFns(..)
  , tokenToTypes
  , tokenToType
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Sugar.Lexer (SourceLocation)
import Sugar.Parser
import Sugar.Types (Quote(NoQuote))

data Type a m c
  = Type'Ty (Ty a)
  | Type'Rec (Rec a m)
  | Type'Adt (Adt a m c)
  deriving (Show, Eq)

data TypeRef = TypeRef
  { trName :: Text
  , trParams :: [TypeRef]
  , trLoc :: SourceLocation
  } deriving (Show, Eq)

data Ty a = Ty
  { tyName :: Text
  , tyParam :: Maybe a
  , tyType :: TypeRef
  , tyLoc :: SourceLocation
  } deriving (Show, Eq)

data Rec a m = Rec
  { recName :: Text
  , recParam :: Maybe a
  , recMems :: [Mem m]
  , recLoc :: SourceLocation
  } deriving (Show, Eq)

data Mem m = Mem
  { memLabel :: Text
  , memParam :: Maybe m
  , memType :: TypeRef
  , memLoc :: SourceLocation
  } deriving (Show, Eq)

data Adt a m c = Adt
  { adtName :: Text
  , adtParam :: Maybe a
  , adtCons :: [Cons m c]
  , adtLoc :: SourceLocation
  } deriving (Show, Eq)

data Cons m c = Cons
  { consTag :: Text
  , consParam :: Maybe c
  , consValue :: ConsValue m
  , consLoc :: SourceLocation
  } deriving (Show, Eq)

data ConsValue m
  = ConsValue'None
  | ConsValue'TypeRef [TypeRef]
  | ConsValue'Mems [Mem m]
  deriving (Show, Eq)

--

data Err e
  = Err'Info e
  | Err'NotTopLevelMap
  | Err'TypeMismatch
  | Err'TypeRefMismatch
  | Err'TypeRefParamsMismatch
  | Err'MemMismatch
  | Err'MemsMismatch
  | Err'ConsMismatch
  deriving (Show, Eq)

data Error e = Error
  { errorLoc :: SourceLocation
  , errorInfo :: Err e
  } deriving (Show, Eq)

data ParamFns a m c e = ParamFns
  { paramFnType :: [TokenStep] -> Either [Error e] a
  , paramFnMem :: [TokenStep] -> Either [Error e]  m
  , paramFnCons :: [TokenStep] -> Either [Error e]  c
  }

tokenToTypes :: ParamFns a m c e -> TokenStep -> Either [Error e] [Type a m c]
tokenToTypes fns (_, Map ts _) = mapWithErrs (uncurry $ tokenToType fns) ts
tokenToTypes _ (loc, _) = Left [Error loc Err'NotTopLevelMap]

tokenToType :: ParamFns a m c e -> TokenStep -> TokenStep -> Either [Error e] (Type a m c)
tokenToType fns n v =
  case Type'Ty <$> tokenToTy fns n v of
    Right ty -> pure ty
    Left tyErrs -> case Type'Rec <$> tokenToRec fns n v of
      Right rec -> pure rec
      Left recErrs -> case Type'Adt <$> tokenToAdt fns n v of
        Right adt -> pure adt
        Left adtErrs -> Left $ betterErrs (betterErrs tyErrs recErrs) adtErrs

betterErrs :: [Error e] -> [Error e] -> [Error e]
betterErrs [Error _ Err'TypeMismatch] ys = ys
betterErrs xs [Error _ Err'TypeMismatch] = xs
betterErrs _ ys = ys -- FIXME: May not be the best fall-through approach

tokenTypeRef :: TokenStep -> Either [Error e] TypeRef
tokenTypeRef (loc, Text name NoQuote Nothing) = Right $ TypeRef (T.pack name) [] loc
tokenTypeRef (loc, Text name NoQuote (Just params)) = TypeRef (T.pack name) <$> (mapWithErrs tokenTypeRef params) <*> pure loc
tokenTypeRef (loc, _) = Left [Error loc Err'TypeRefMismatch]

tokenToTy :: ParamFns a m c e -> TokenStep -> TokenStep -> Either [Error e] (Ty a)
tokenToTy fns (loc, Text t NoQuote params) v = Ty (T.pack t) <$> tokenParams (paramFnType fns) params <*> tokenTypeRef v <*> pure loc
tokenToTy _ _ (loc, _) = Left [Error loc Err'TypeMismatch]

tokenToRec :: ParamFns a m c e -> TokenStep -> TokenStep -> Either [Error e] (Rec a m)
tokenToRec fns (loc, Text name NoQuote params) v = Rec (T.pack name) <$> tokenParams (paramFnType fns) params <*> tokenToMems fns v <*> pure loc
tokenToRec  _ _ (loc, _) = Left [Error loc Err'TypeMismatch]

tokenToMems :: ParamFns a m c e -> TokenStep -> Either [Error e] [Mem m]
tokenToMems fns (_, Map mems Nothing) = tokenToMems' fns mems
tokenToMems _ (loc, _) = Left [Error loc Err'MemsMismatch]

tokenToMems' :: ParamFns a m c e -> [(TokenStep,TokenStep)] -> Either [Error e] [Mem m]
tokenToMems' fns = mapWithErrs (uncurry $ tokenToMem fns)

tokenToMem :: ParamFns a m c e -> TokenStep -> TokenStep -> Either [Error e] (Mem m)
tokenToMem fns (loc, Text name NoQuote params) s = Mem (T.pack name) <$> tokenParams (paramFnMem fns) params <*> tokenTypeRef s <*> pure loc
tokenToMem _ _ (loc, _) = Left [Error loc Err'MemMismatch]

tokenToAdt :: ParamFns a m c e -> TokenStep -> TokenStep -> Either [Error e] (Adt a m c)
tokenToAdt fns (loc, Text name NoQuote params) (_, List conss _ Nothing) = Adt (T.pack name) <$> tokenParams (paramFnType fns) params <*> mapWithErrs (tokenToCons fns) conss <*> pure loc
tokenToAdt _ _ (loc, _) = Left [Error loc Err'TypeMismatch]

tokenToCons :: ParamFns a m c e -> TokenStep -> Either [Error e] (Cons m c)
tokenToCons _ (loc, Text ('&':name) NoQuote params) = Cons (T.pack name) <$> pure Nothing <*> ((ConsValue'TypeRef . pure) <$> (tokenTypeRef cons)) <*> pure loc
  where cons = (loc, Text name NoQuote params)
tokenToCons fns (loc, Text name NoQuote params) = Cons (T.pack name) <$> tokenParams (paramFnCons fns) params <*> pure ConsValue'None <*> pure loc
tokenToCons fns (loc, List [(_, Text name NoQuote Nothing)] _ params) = Cons (T.pack name) <$> tokenParams (paramFnCons fns) params <*> pure ConsValue'None <*> pure loc
tokenToCons fns (loc, List [(_, Text name NoQuote Nothing), (_, Map mems Nothing)] _ params) = do
  ms <- tokenToMems' fns mems
  param <- tokenParams (paramFnCons fns) params
  return $ Cons
    (T.pack name)
    param
    (if null ms
      then ConsValue'None
      else ConsValue'Mems ms)
    loc
tokenToCons fns (loc, (List ((_, Text name NoQuote Nothing):xs) _ params)) = Cons (T.pack name) <$> tokenParams (paramFnCons fns) params <*> (ConsValue'TypeRef <$> mapWithErrs tokenTypeRef xs) <*> pure loc
tokenToCons _ (loc,_) = Left [Error loc Err'ConsMismatch]

mapWithErrs :: (a -> Either [Error e] b) -> [a] -> Either [Error e] [b]
mapWithErrs _ [] = Right []
mapWithErrs f (x:xs) = case f x of
  Left es -> case mapWithErrs f xs of
    Left es' -> Left $ es ++ es'
    Right _ -> Left es
  Right y -> case mapWithErrs f xs of
    Left es -> Left es
    Right ys -> Right $ y : ys

tokenParams :: ([TokenStep] -> Either [Error e] a) -> Maybe [TokenStep] -> Either [Error e] (Maybe a)
tokenParams f params = case params of
  Nothing -> pure Nothing
  Just ps -> Just <$> f ps
