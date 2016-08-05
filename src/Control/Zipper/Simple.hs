{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Zipper.Simple (
  Root,
  type (==>),
  type (=*=>),
  Rooted (..),
  Focused (..),
  Ascend (..),
  root,
  descendLens,
  descendPrism,
  descendList,
  leftward,
  rightward,
  deleteFocus
) where

import Control.Lens

newtype Root a = Root { unroot :: a }

infixl 8 ==>
data z ==> a =
  One (a -> z) a

infixl 8 =*=>
data z =*=> a =
  Many ([a] -> z) [a] [a] a

class Focused z where
  type FocusedAt z

  focus :: Lens' z (FocusedAt z)

instance Focused (Root a) where
  type FocusedAt (Root a) = a

  focus :: Lens' (Root a) a
  focus f = fmap Root . f . unroot

instance Focused (z ==> a) where
  type FocusedAt (z ==> a) = a

  focus :: Lens' (z ==> a) a
  focus f (One g x) = fmap (One g) $ f x

instance Focused (z =*=> a) where
  type FocusedAt (z =*=> a) = a

  focus :: Lens' (z =*=> a) a
  focus f (Many g l r x) = fmap (Many g l r) $ f x

class Ascend z where
  type BuildsOn z

  ascend :: z -> BuildsOn z

instance Ascend (z ==> a) where
  type BuildsOn (z ==> a) = z

  ascend :: z ==> a -> z
  ascend (One f x) = f x

instance Ascend (z =*=> a) where
  type BuildsOn (z =*=> a) = z

  ascend :: z =*=> a -> z
  ascend (Many f l r x) = f $ reverse l ++ [x] ++ r

class Rooted z where
  type RootedAt z

  rezip :: z -> RootedAt z

  default rezip :: (Rooted (BuildsOn z), Ascend z) => z -> RootedAt (BuildsOn z)
  rezip = rezip . ascend

instance Rooted (Root a) where
  type RootedAt (Root a) = a

  rezip :: Root a -> a
  rezip (Root x) = x

instance Rooted z => Rooted (z ==> x) where
  type RootedAt (z ==> x) = RootedAt z

instance Rooted z => Rooted (z =*=> x) where
  type RootedAt (z =*=> a) = RootedAt z

root :: a -> Root a
root = Root

descendLens :: Focused z => Lens' (FocusedAt z) a -> z -> z ==> a
descendLens l z =
  One (\x -> set (focus . l) x z) $ view (focus . l) z

descendPrism :: Focused z => Prism' (FocusedAt z) a -> z -> Maybe (z ==> a)
descendPrism p z =
  case preview (focus . p) z of
    Nothing -> Nothing
    Just x -> Just $ One (flip (set focus) z . review p) x

descendList :: (Focused z, FocusedAt z ~ [a]) => z -> Maybe (z =*=> a)
descendList z =
  case view focus z of
    [] -> Nothing
    (x:xs) ->
      Just $ Many (\vs -> set focus vs z) [] xs x

leftward :: z =*=> a -> Maybe (z =*=> a)
leftward (Many _ [] _ _) = Nothing
leftward (Many f (l:ls) r x) = Just $ Many f ls (x:r) l

rightward :: z =*=> a -> Maybe (z =*=> a)
rightward (Many _ _ [] _) = Nothing
rightward (Many f l (r:rs) x) = Just $ Many f (x:l) rs r

deleteFocus :: z =*=> a -> Either z (z =*=> a)
deleteFocus (Many f [] [] _) = Left $ f []
deleteFocus (Many f l (r:rs) _) = Right $ Many f l rs r
deleteFocus (Many f (l:ls) _ _) = Right $ Many f ls [] l
