{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Sugar.Data where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String (IsString(..))
import Control.Applicative ((<|>))
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

data ParamFns a m c = ParamFns
  { paramFnType :: [Sugar] -> Maybe a
  , paramFnMem :: [Sugar] -> Maybe m
  , paramFnCons :: [Sugar] -> Maybe c
  }

sugarToTypes :: ParamFns a m c -> Sugar -> Maybe [Type a m c]
sugarToTypes fns (Sugar'Map ts _) = mapM (uncurry $ sugarToType fns) ts
sugarToTypes _ _ = Nothing

sugarToType :: ParamFns a m c -> Sugar -> Sugar -> Maybe (Type a m c)
sugarToType fns n v =
  (Type'Ty <$> sugarToTy fns n v) <|>
  (Type'Rec <$> sugarToRec fns n v) <|>
  (Type'Adt <$> sugarToAdt fns n v)

sugarTypeRef :: Sugar -> Maybe TypeRef
sugarTypeRef (Sugar'Text name Nothing) = Just $ TypeRef name []
sugarTypeRef (Sugar'Text name (Just params)) = TypeRef name <$> (mapM sugarTypeRef params)
sugarTypeRef _ = Nothing

sugarTypeRefParams :: Sugar -> Maybe [TypeRef]
sugarTypeRefParams (Sugar'Text name Nothing) = Just [TypeRef name []]
sugarTypeRefParams (Sugar'Text name (Just params)) = (\x -> [TypeRef name x]) <$> (mapM sugarTypeRef params)
sugarTypeRefParams (Sugar'List params _ Nothing) = mapM sugarTypeRef params
sugarTypeRefParams _ = Nothing

sugarToTy :: ParamFns a m c -> Sugar -> Sugar -> Maybe (Ty a)
sugarToTy fns (Sugar'Text t params) v = Ty t (paramFnType fns =<< params) <$> sugarTypeRef v
sugarToTy _ _ _ = Nothing

sugarToRec :: ParamFns a m c -> Sugar -> Sugar -> Maybe (Rec a m)
sugarToRec fns (Sugar'Text name Nothing) v = Rec name Nothing <$> sugarToMems fns v
sugarToRec fns (Sugar'Text name (Just params)) v = Rec name <$> (Just <$> paramFnType fns params) <*> sugarToMems fns v
sugarToRec  _ _ _ = Nothing

sugarToMems :: ParamFns a m c -> Sugar -> Maybe [Mem m]
sugarToMems fns (Sugar'Map mems Nothing) = sugarToMems' fns mems
sugarToMems _ _ = Nothing

sugarToMems' :: ParamFns a m c -> [(Sugar,Sugar)] -> Maybe [Mem m]
sugarToMems' fns = mapM (uncurry $ sugarToMem fns)

sugarToMem :: ParamFns a m c -> Sugar -> Sugar -> Maybe (Mem m)
sugarToMem _ (Sugar'Text name Nothing) s = Mem name Nothing <$> sugarTypeRef s
sugarToMem fns (Sugar'Text name (Just params)) s = Mem name <$> (Just <$> paramFnMem fns params) <*> sugarTypeRef s
sugarToMem _ _ _ = Nothing

sugarToAdt :: ParamFns a m c -> Sugar -> Sugar -> Maybe (Adt a m c)
sugarToAdt fns (Sugar'Text name Nothing) (Sugar'List conss _ Nothing) = Adt name Nothing <$> mapM (sugarToCons fns) conss
sugarToAdt fns (Sugar'Text name (Just params)) (Sugar'List conss _ Nothing) = Adt name <$> (Just <$> paramFnType fns params) <*> mapM (sugarToCons fns) conss
sugarToAdt _ _ _ = Nothing

sugarToCons :: ParamFns a m c -> Sugar -> Maybe (Cons m c)
sugarToCons _ (Sugar'Text name Nothing) = Just $ Cons name Nothing ConsValue'None
sugarToCons fns (Sugar'Text name (Just params)) =  Cons name <$> (Just <$> paramFnCons fns params) <*> pure ConsValue'None
sugarToCons _ (Sugar'List [(Sugar'Text name Nothing)] _ Nothing) = Just $ Cons name Nothing ConsValue'None
sugarToCons fns (Sugar'List [(Sugar'Text name Nothing)] _ (Just params)) = Cons name <$> (Just <$> paramFnCons fns params) <*> pure ConsValue'None
sugarToCons fns (Sugar'List [(Sugar'Text name Nothing), (Sugar'Map mems Nothing)] _ Nothing) = do
  ms <- sugarToMems' fns mems
  return . Cons name Nothing $ if null ms
    then ConsValue'None
    else ConsValue'Mems ms
sugarToCons _ (Sugar'List ((Sugar'Text name Nothing):xs) _ Nothing) = Cons name Nothing <$> (ConsValue'TypeRef <$> mapM sugarTypeRef xs)
sugarToCons fns (Sugar'List ((Sugar'Text name Nothing):xs) _ (Just params)) = Cons name <$> (Just <$> paramFnCons fns params) <*> (ConsValue'TypeRef <$> mapM sugarTypeRef xs)
sugarToCons _ _ = Nothing
