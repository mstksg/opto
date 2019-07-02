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
  , traverseZP, traverseZP_, mapZP
  , traverseZP1, traverseZP2
  -- , zipZipProd
  ) where

-- import           Data.Type.Combinator
-- import           Data.Type.Conjunction
-- import           Data.Type.Product
import           Data.Functor.Identity
import           Data.Kind
import           Data.Vinyl.Core

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

traverseZP_
    :: forall h f as bs. Applicative h
    => (forall x y. f x y -> h ())
    -> ZipProd f as bs
    -> h ()
traverseZP_ f = go
  where
    go :: ZipProd f cs ds -> h ()
    go = \case
      ZPØ      -> pure ()
      x :<< xs -> f x *> go xs

mapZP
    :: (forall x y. f x y -> g x y)
    -> ZipProd f as bs
    -> ZipProd g as bs
mapZP f = runIdentity . traverseZP (Identity . f)

traverseZP1
    :: forall h f g as bs. Applicative h
    => (forall x y. f x y -> h (g x))
    -> ZipProd f as bs
    -> h (Rec g as)
traverseZP1 f = go
  where
    go :: ZipProd f cs ds -> h (Rec g cs)
    go = \case
      ZPØ      -> pure RNil
      x :<< xs -> (:&) <$> f x <*> go xs

traverseZP2
    :: forall h f g as bs. Applicative h
    => (forall x y. f x y -> h (g y))
    -> ZipProd f as bs
    -> h (Rec g bs)
traverseZP2 f = go
  where
    go :: ZipProd f cs ds -> h (Rec g ds)
    go = \case
      ZPØ      -> pure RNil
      x :<< xs -> (:&) <$> f x <*> go xs

-- zipZipProd
--     :: ZipProd f as bs
--     -> ZipProd g as bs
--     -> ZipProd (Cur (Uncur f :&: Uncur g)) as bs
-- zipZipProd = \case
--     ZPØ -> \case
--       ZPØ -> ZPØ
--     x :<< xs -> \case
--       y :<< ys -> Cur (Uncur x :&: Uncur y) :<< zipZipProd xs ys
