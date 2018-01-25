{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Type.ZipProd (
    ZipProd(..)
  , onlyZP, headZP, tailZP
  , traverseZP, mapZP
  , traverseZP1, traverseZP2
  ) where

import           Data.Kind
import           Data.Type.Combinator
import           Data.Type.Product

data ZipProd :: (Type -> Type -> Type) -> [Type] -> [Type] -> Type where
    ZPØ   :: ZipProd f '[] '[]
    (:<<) :: f a b -> ZipProd f as bs -> ZipProd f (a ': as) (b ': bs)
infixr 5 :<<

onlyZP :: f a b -> ZipProd f '[a] '[b]
onlyZP = (:<< ZPØ)

headZP :: ZipProd f (a ': as) (b ': bs) -> f a b
headZP (x :<< _) = x

tailZP :: ZipProd f (a ': as) (b ': bs) -> ZipProd f as bs
tailZP (_ :<< xs) = xs

traverseZP
    :: forall h f g as bs. Applicative h
    => (forall x y. f x y -> h (g x y))
    -> ZipProd f as bs
    -> h (ZipProd g as bs)
traverseZP f = go
  where
    go :: ZipProd f cs ds -> h (ZipProd g cs ds)
    go = \case
      ZPØ      -> pure ZPØ
      x :<< xs -> (:<<) <$> f x <*> go xs

mapZP
    :: (forall x y. f x y -> g x y)
    -> ZipProd f as bs
    -> ZipProd g as bs
mapZP f = getI . traverseZP (I . f)

traverseZP1
    :: forall h f g as bs. Applicative h
    => (forall x y. f x y -> h (g x))
    -> ZipProd f as bs
    -> h (Prod g as)
traverseZP1 f = go
  where
    go :: ZipProd f cs ds -> h (Prod g cs)
    go = \case
      ZPØ      -> pure Ø
      x :<< xs -> (:<) <$> f x <*> go xs

traverseZP2
    :: forall h f g as bs. Applicative h
    => (forall x y. f x y -> h (g y))
    -> ZipProd f as bs
    -> h (Prod g bs)
traverseZP2 f = go
  where
    go :: ZipProd f cs ds -> h (Prod g ds)
    go = \case
      ZPØ      -> pure Ø
      x :<< xs -> (:<) <$> f x <*> go xs
