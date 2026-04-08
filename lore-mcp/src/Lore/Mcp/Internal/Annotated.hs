{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Lore.Mcp.Internal.Annotated where

import Control.Lens ((%~), (&), (?~))
import qualified Data.Aeson as J
import Data.Data (Proxy (..), Typeable)
import Data.Kind (Type)
import Data.OpenApi (HasSchema (..))
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..), toInlinedSchema)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified GHC.TypeError as TE
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Natural, Symbol, natVal, symbolVal)

data FieldType = ValueType | MetadataType

data FieldMetadata a (metadata :: [Type])

type Meta xs = xs

type family Field (t :: k1) (a :: k2) :: Type where
  Field 'ValueType (a :: Type) = a
  Field 'MetadataType (Maybe a :: Type) = Maybe (FieldMetadata a '[])
  Field 'MetadataType (a :: Type) = FieldMetadata a '[]

type family WithMeta (t :: k1) (a :: k2) :: Type where
  WithMeta (FieldMetadata a (oldMetadata :: [Type])) (newMetadata :: [Type]) = FieldMetadata a (oldMetadata ++ newMetadata)
  WithMeta (Maybe (FieldMetadata a (oldMetadata :: [Type]))) (newMetadata :: [Type]) = Maybe (FieldMetadata a (oldMetadata ++ newMetadata))
  WithMeta a b = a

data Description (description :: Symbol)
  deriving (Typeable)

data Example (example :: k)
  deriving (Typeable)

data ExampleList (example :: [k])
  deriving (Typeable)

type family CanBeExample a example where
  CanBeExample (Maybe a) (Proxy example) = CanBeExample a (Proxy example)
  CanBeExample Int (Proxy (example :: Natural)) = 'True
  CanBeExample T.Text (Proxy (example :: Symbol)) = 'True
  CanBeExample String (Proxy (example :: Symbol)) = 'True
  CanBeExample a (Proxy example) = TE.TypeError (TE.Text "Example issue: " TE.:<>: TE.ShowType example TE.:<>: TE.Text " can not be used as an example for " TE.:<>: TE.ShowType a TE.:<>: TE.Text ". \n Check the metadata of the type or update the type family CanBeExample to support this type.")

class (CanBeExample a (Proxy example) ~ 'True) => IsExample a example where
  exampleToJSON :: J.Value

instance (KnownSymbol example, CanBeExample a (Proxy example) ~ 'True) => IsExample a (example :: Symbol) where
  exampleToJSON = J.toJSON $ symbolVal (Proxy @example)

instance (KnownNat example, CanBeExample a (Proxy example) ~ 'True) => IsExample a (example :: Nat) where
  exampleToJSON = J.toJSON $ natVal (Proxy @example)

data Minimum (minimum :: Nat)
  deriving (Typeable)

data Maximum (maximum :: Nat)
  deriving (Typeable)

class IsFieldMetadata a metadata where
  modifySchema :: OpenApi.Schema -> OpenApi.Schema

instance (KnownNat minimum) => IsFieldMetadata a (Minimum minimum) where
  modifySchema schema' = schema' & OpenApi.minimum_ ?~ fromIntegral (natVal (Proxy @minimum))

instance (KnownNat maximum) => IsFieldMetadata a (Maximum maximum) where
  modifySchema schema' = schema' & OpenApi.maximum_ ?~ fromIntegral (natVal (Proxy @maximum))

instance (KnownSymbol description) => IsFieldMetadata a (Description description) where
  modifySchema schema' = schema' & OpenApi.description ?~ T.pack (symbolVal (Proxy @description))

instance (IsExample a example) => IsFieldMetadata a (Example example) where
  modifySchema schema' = schema' & OpenApi.example ?~ exampleToJSON @a @example

instance IsFieldMetadata [a] (ExampleList '[]) where
  modifySchema schema' = schema'

instance (IsFieldMetadata [a] (ExampleList rest), IsExample a example) => IsFieldMetadata [a] (ExampleList (example ': rest)) where
  modifySchema schema' = modifySchema @[a] @(ExampleList rest) modifiedSchema
    where
      modifiedSchema =
        schema'
          & OpenApi.example %~ \case
            Just (J.Array xs) -> Just $ J.Array $ xs <> Vector.singleton (exampleToJSON @a @example)
            _ -> Just $ J.Array $ Vector.singleton (exampleToJSON @a @example)

instance (IsFieldMetadata a (ExampleList xs)) => IsFieldMetadata (Maybe a) (ExampleList xs) where
  modifySchema = modifySchema @a @(ExampleList xs)

instance IsFieldMetadata a '[] where
  modifySchema schema' = schema'

instance (IsFieldMetadata a m, IsFieldMetadata a rest) => IsFieldMetadata a (m ': rest) where
  modifySchema schema' = modifySchema @a @m (modifySchema @a @rest schema')

instance (ToSchema a, IsFieldMetadata a metadata, Typeable metadata) => ToSchema (FieldMetadata a metadata) where
  declareNamedSchema _ = do
    namedSchema <- declareNamedSchema (Proxy @a)
    pure $ namedSchema & schema %~ modifySchema @a @metadata

data MinItems (n :: Nat)
  deriving (Typeable)

data MaxItems (n :: Nat)
  deriving (Typeable)

instance (KnownNat n) => IsFieldMetadata [a] (MinItems n) where
  modifySchema schema' = schema' & OpenApi.minItems ?~ natVal (Proxy @n)

instance (KnownNat n) => IsFieldMetadata [a] (MaxItems n) where
  modifySchema schema' = schema' & OpenApi.maxItems ?~ natVal (Proxy @n)

proxyToValueType :: forall req. Proxy req -> Proxy (req 'ValueType)
proxyToValueType _ = Proxy

proxyToMetadataType :: forall req. Proxy req -> Proxy (req 'MetadataType)
proxyToMetadataType _ = Proxy

getObjectSchema :: forall req. (ToSchema (req 'MetadataType)) => Proxy req -> OpenApi.Schema
getObjectSchema = toInlinedSchema . proxyToMetadataType

convertFromJSON :: forall req. (J.FromJSON (req 'ValueType)) => Proxy req -> J.Value -> Either String (req 'ValueType)
convertFromJSON _ a = case J.fromJSON @(req 'ValueType) a of
  J.Error e -> Left e
  J.Success res -> Right res

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
