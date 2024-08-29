{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Config.Suckless.Syntax
  ( Syntax(..)
  , Id(..)
  , Literal(..)
  , HasContext
  , C(..)
  , Context(..)
  , IsContext(..)
  , IsLiteral(..)
  , pattern SymbolVal
  , pattern ListVal
  , pattern LitIntVal
  , pattern LitStrVal
  , pattern LitBoolVal
  , pattern LitScientificVal
  , pattern StringLike
  , pattern StringLikeList
  , pattern Nil
  )
  where

import Data.Data
import Data.Kind
import Data.String
import Data.Text (Text)
import Data.Scientific
import GHC.Generics (Generic(..))
import Data.Maybe
-- import GHC.Generics( Fixity(..) )
-- import Data.Data as Data
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson
import Data.Vector qualified as V
import Data.Traversable (forM)
import Data.Text qualified as Text
import Data.Function

import Prettyprinter

pattern SymbolVal :: Id -> Syntax c
pattern SymbolVal v <- Symbol _ v

-- pattern LitVal :: forall {c}. Id -> Li
pattern LitIntVal :: Integer -> Syntax c
pattern LitIntVal v <- Literal _ (LitInt v)

pattern LitScientificVal :: Scientific -> Syntax c
pattern LitScientificVal v <- Literal _ (LitScientific v)

pattern LitStrVal :: Text -> Syntax c
pattern LitStrVal v <- Literal _ (LitStr v)

pattern LitBoolVal :: Bool -> Syntax c
pattern LitBoolVal v <- Literal _ (LitBool v)

pattern ListVal :: [Syntax c] -> Syntax c
pattern ListVal v <- List _ v


stringLike :: Syntax c -> Maybe String
stringLike = \case
  LitStrVal s -> Just $ Text.unpack s
  SymbolVal (Id s) -> Just $ Text.unpack s
  _ -> Nothing

stringLikeList :: [Syntax c] -> [String]
stringLikeList syn = [ stringLike s | s <- syn ] & takeWhile isJust & catMaybes


pattern StringLike :: forall {c} . String -> Syntax c
pattern StringLike e <- (stringLike -> Just e)

pattern StringLikeList :: forall {c} . [String] -> [Syntax c]
pattern StringLikeList e <- (stringLikeList -> e)


pattern Nil :: forall {c} . Syntax c
pattern Nil <- ListVal []



data family Context c :: Type

class IsContext c where
  noContext :: Context c

data instance Context () = EmptyContext

instance IsContext () where
  noContext = EmptyContext

class HasContext c a where

class IsLiteral a where
  mkLit :: a -> Literal

newtype Id =
  Id Text
  deriving newtype (IsString,Pretty)
  deriving stock (Data,Generic,Show,Eq,Ord)

data Literal =
    LitStr   Text
  | LitInt   Integer
  | LitScientific Scientific
  | LitBool  Bool
  deriving stock (Eq,Ord,Data,Generic,Show)

instance IsLiteral Text where
  mkLit = LitStr

instance IsLiteral Bool where
  mkLit = LitBool

instance IsLiteral Integer where
  mkLit = LitInt

data C = C
         deriving stock (Eq,Ord,Show,Data,Typeable,Generic)

-- simple, yet sufficient context
--  Integer may be offset, maybe line number,
--  token number, whatever
--  it's up to parser to use this context for
--  error printing, etc
newtype instance (Context C) =
  SimpleContext { fromSimpleContext :: Maybe Integer }
  deriving stock (Eq,Ord,Show,Data,Typeable,Generic)

instance IsContext C where
  noContext = SimpleContext Nothing

data Syntax c
  = List    (Context c) [Syntax c]
  | Symbol  (Context c) Id
  | Literal (Context c) Literal
  deriving stock (Generic,Typeable)


instance Eq (Syntax c) where
  (==) (Literal _ a) (Literal _ b) = a == b
  (==) (Symbol _ a)  (Symbol _ b) = a == b
  (==) (List _ a)    (List _ b) = a == b
  (==) _ _ = False

deriving instance (Data c, Data (Context c)) => Data (Syntax c)

instance Pretty (Syntax c) where
  pretty (Literal _ ast) = pretty ast
  pretty (Symbol _ s)    = pretty s
  pretty (List _ (x:xs)) = parens $ align $ sep ( fmap pretty (x:xs) )
  pretty (List _ [])     = parens mempty

instance Pretty Literal where
  pretty = \case
    LitStr s  -> dquotes (pretty s)
    LitInt i  -> pretty i
    LitScientific v  -> viaShow v

    LitBool b | b          -> "#t"
              | otherwise  -> "#f"


instance ToJSON Literal where
    toJSON (LitStr s)       = String s
    toJSON (LitInt i)       = Number (fromInteger i)
    toJSON (LitScientific s) = Number s
    toJSON (LitBool b)      = Bool b

instance ToJSON (Syntax c) where
    toJSON (Symbol _ (Id "#nil")) = Null
    toJSON (Symbol _ (Id s)) = String s
    toJSON (Literal _ l) = toJSON l
    toJSON (List _ items) =
        case items of
            (Symbol _ "object" : rest) ->
                object $ mapMaybe pairToKeyValue rest
            _ -> Array . V.fromList $ fmap toJSON items

      where
        pairToKeyValue :: Syntax c -> Maybe (Key, Value)
        pairToKeyValue (List _ [SymbolVal (Id k), SymbolVal ":", v]) = Just (fromText k .= toJSON v)
        pairToKeyValue (List _ [LitStrVal k, SymbolVal ":", v]) = Just (fromText k .= toJSON v)
        pairToKeyValue _ = Nothing


instance IsContext c => FromJSON (Syntax c) where
    parseJSON (String t) = pure $ Literal noContext (LitStr t)
    parseJSON (Number n)
        | isInteger n = pure $ Literal noContext (LitInt (floor n))
        | otherwise   = pure $ Literal noContext (LitScientific n)
    parseJSON (Bool b)  = pure $ Literal noContext (LitBool b)
    parseJSON (Array a) = List noContext <$> mapM parseJSON (V.toList a)
    parseJSON (Object o) = do
        pairs <- forM (Aeson.toList o) $ \(key, value) -> do
            valueSyntax <- parseJSON value
            pure $ List noContext [ Symbol noContext (Id (toText key))
                                  , Symbol noContext ":"
                                  , valueSyntax
                                  ]
        pure $ List noContext (Symbol noContext (Id "object") : pairs)
    parseJSON _ = fail "Cannot parse JSON to Syntax"


