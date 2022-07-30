{-# LANGUAGE OverloadedStrings #-}
module Sugar.Data where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String (IsString(..))
import Sugar

data Type a m c
  = Type'Ty (Ty a)
  | Type'Rec (Rec a m)
  | Type'Adt (Adt a m c)
  deriving (Show, Eq)

data TypeRef = TypeRef
  { trName :: Text
  , trParams :: [TypeRef]
  } deriving (Show, Eq)

instance IsString TypeRef where
  fromString s = TypeRef (T.pack s) []

data Ty a = Ty
  { tyName :: Text
  , tyParam :: Maybe a
  , tyType :: TypeRef
  } deriving (Show, Eq)

data Rec a m = Rec
  { recName :: Text
  , recParam :: Maybe a
  , recMems :: [Mem m]
  } deriving (Show, Eq)

data Mem m = Mem
  { memLabel :: Text
  , memParam :: Maybe m
  , memType :: TypeRef
  } deriving (Show, Eq)

data Adt a m c = Adt
  { adtName :: Text
  , adtParam :: Maybe a
  , adtCons :: [Cons m c]
  } deriving (Show, Eq)

data Cons m c = Cons
  { consTag :: Text
  , consParam :: Maybe c
  , consValue :: ConsValue m
  } deriving (Show, Eq)

data ConsValue m
  = ConsValue'None
  | ConsValue'TypeRef [TypeRef]
  | ConsValue'Mems [Mem m]
  deriving (Show, Eq)

--

data Err e
  = Err'Err e
  | Err'TypesNeedMap
  | Err'TypeMismatch
  | Err'TypeRefMismatch
  | Err'TypeRefParamsMismatch
  | Err'MemMismatch
  | Err'MemsMismatch
  | Err'ConsMismatch
  deriving (Show, Eq)

data ParamFns a m c e = ParamFns
  { paramFnType :: [Sugar] -> Either [Err e] a
  , paramFnMem :: [Sugar] -> Either [Err e]  m
  , paramFnCons :: [Sugar] -> Either [Err e]  c
  }

sugarToTypes :: ParamFns a m c e -> Sugar -> Either [Err e] [Type a m c]
sugarToTypes fns (Sugar'Map ts _) = mapWithErrs (uncurry $ sugarToType fns) ts
sugarToTypes _ _ = Left [Err'TypesNeedMap]

sugarToType :: ParamFns a m c e -> Sugar -> Sugar -> Either [Err e] (Type a m c)
sugarToType fns n v =
  case Type'Ty <$> sugarToTy fns n v of
    Right ty -> pure ty
    Left tyErrs -> case Type'Rec <$> sugarToRec fns n v of
      Right rec -> pure rec
      Left recErrs -> case Type'Adt <$> sugarToAdt fns n v of
        Right adt -> pure adt
        Left adtErrs -> Left $ betterErrs (betterErrs tyErrs recErrs) adtErrs

betterErrs :: [Err e] -> [Err e] -> [Err e]
betterErrs [Err'TypeMismatch] ys = ys
betterErrs xs [Err'TypeMismatch] = xs
betterErrs _ ys = ys -- FIXME: May not be the best fall-through approach

sugarTypeRef :: Sugar -> Either [Err e] TypeRef
sugarTypeRef (Sugar'Text name Nothing) = Right $ TypeRef name []
sugarTypeRef (Sugar'Text name (Just params)) = TypeRef name <$> (mapWithErrs sugarTypeRef params)
sugarTypeRef _ = Left [Err'TypeRefMismatch]

sugarTypeRefParams :: Sugar -> Either [Err e] [TypeRef]
sugarTypeRefParams (Sugar'Text name Nothing) = Right [TypeRef name []]
sugarTypeRefParams (Sugar'Text name (Just params)) = (\x -> [TypeRef name x]) <$> (mapWithErrs sugarTypeRef params)
sugarTypeRefParams (Sugar'List params _ Nothing) = mapWithErrs sugarTypeRef params
sugarTypeRefParams _ = Left [Err'TypeRefParamsMismatch]

sugarToTy :: ParamFns a m c e -> Sugar -> Sugar -> Either [Err e] (Ty a)
sugarToTy fns (Sugar'Text t params) v = Ty t <$> sugarParams (paramFnType fns) params <*> sugarTypeRef v
sugarToTy _ _ _ = Left [Err'TypeMismatch]

sugarToRec :: ParamFns a m c e -> Sugar -> Sugar -> Either [Err e] (Rec a m)
sugarToRec fns (Sugar'Text name params) v = Rec name <$> sugarParams (paramFnType fns) params <*> sugarToMems fns v
sugarToRec  _ _ _ = Left [Err'TypeMismatch]

sugarToMems :: ParamFns a m c e -> Sugar -> Either [Err e] [Mem m]
sugarToMems fns (Sugar'Map mems Nothing) = sugarToMems' fns mems
sugarToMems _ _ = Left [Err'MemsMismatch]

sugarToMems' :: ParamFns a m c e -> [(Sugar,Sugar)] -> Either [Err e] [Mem m]
sugarToMems' fns = mapWithErrs (uncurry $ sugarToMem fns)

sugarToMem :: ParamFns a m c e -> Sugar -> Sugar -> Either [Err e] (Mem m)
sugarToMem fns (Sugar'Text name params) s = Mem name <$> sugarParams (paramFnMem fns) params <*> sugarTypeRef s
sugarToMem _ _ _ = Left [Err'MemMismatch]

sugarToAdt :: ParamFns a m c e -> Sugar -> Sugar -> Either [Err e] (Adt a m c)
sugarToAdt fns (Sugar'Text name params) (Sugar'List conss _ Nothing) = Adt name <$> sugarParams (paramFnType fns) params <*> mapWithErrs (sugarToCons fns) conss
sugarToAdt _ _ _ = Left [Err'TypeMismatch]

sugarToCons :: ParamFns a m c e -> Sugar -> Either [Err e] (Cons m c)
sugarToCons fns (Sugar'Text name params) =  Cons name <$> sugarParams (paramFnCons fns) params <*> pure ConsValue'None
sugarToCons fns (Sugar'List [(Sugar'Text name Nothing)] _ params) = Cons name <$> sugarParams (paramFnCons fns) params <*> pure ConsValue'None
sugarToCons fns (Sugar'List [(Sugar'Text name Nothing), (Sugar'Map mems Nothing)] _ params) = do
  ms <- sugarToMems' fns mems
  param <- sugarParams (paramFnCons fns) params
  return . Cons name param $ if null ms
    then ConsValue'None
    else ConsValue'Mems ms
sugarToCons fns (Sugar'List ((Sugar'Text name Nothing):xs) _ params) = Cons name <$> sugarParams (paramFnCons fns) params <*> (ConsValue'TypeRef <$> mapWithErrs sugarTypeRef xs)
sugarToCons _ _ = Left [Err'ConsMismatch]

mapWithErrs :: (a -> Either [Err e] b) -> [a] -> Either [Err e] [b]
mapWithErrs _ [] = Right []
mapWithErrs f (x:xs) = case f x of
  Left es -> case mapWithErrs f xs of
    Left es' -> Left $ es ++ es'
    Right _ -> Left es
  Right y -> case mapWithErrs f xs of
    Left es -> Left es
    Right ys -> Right $ y : ys

sugarParams :: ([Sugar] -> Either [Err e] a) -> Maybe [Sugar] -> Either [Err e] (Maybe a)
sugarParams f params = case params of
  Nothing -> pure Nothing
  Just ps -> Just <$> f ps
