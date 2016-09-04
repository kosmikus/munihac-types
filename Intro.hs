{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Intro where

data QuestionType = TYesNo | TNumeric

data Questions sig = Questions (Env Question sig)

data Question (qt :: QuestionType) where
  QYesNo :: String -> Bool -> Question 'TYesNo
  QNumeric :: String -> Int -> Question 'TNumeric

data Answers sig = Answers (Env Answer sig)

data Answer (qt :: QuestionType) where
  AYesNo :: Bool -> Answer 'TYesNo
  ANumeric :: Int -> Answer 'TNumeric

example :: Questions '[ 'TYesNo, 'TNumeric]
example = Questions $
     QYesNo "Ist heute Sonntag?" True
  :* QNumeric "Wieviele Leute beim MuniHac?" 100
  :* ENil

answers :: Answers '[ 'TYesNo, 'TNumeric]
answers = Answers $
     AYesNo True
  :* ANumeric 95
  :* ENil

combineFunction :: Question qt -> Answer qt -> K Int qt
combineFunction (QYesNo _ correct) (AYesNo b) =
  if correct == b then K 10 else K 0
combineFunction (QNumeric _ n) (ANumeric i) = K (10 - abs (n - i))

data K a b = K a

score :: Questions sig -> Answers sig -> Int
score (Questions qs) (Answers as) =
  let v = eZipWith
            combineFunction
            qs
            as
  in sum (eToList v)

data Env (f :: k -> *) (sig :: [k]) where
  ENil :: Env f '[]
  (:*) :: f a -> Env f sig -> Env f (a ': sig)

infixr 5 :*

eZipWith ::
  (forall a . f a -> g a -> h a) -> Env f sig -> Env g sig -> Env h sig
eZipWith _op ENil      ENil      = ENil
eZipWith  op (x :* xs) (y :* ys) = op x y :* eZipWith op xs ys

data HList (sig :: [*]) where
  HNil :: HList '[]
  (:-) :: a -> HList sig -> HList (a ': sig)

infixr 5 :-

data Nat = Zero | Suc Nat

data Vec (n :: Nat) (a :: *) where
  Nil  :: Vec 'Zero a
  (:.) :: a -> Vec n a -> Vec ('Suc n) a

infixr 5 :.

vZipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vZipWith _op Nil       Nil       = Nil
vZipWith  op (x :. xs) (y :. ys) = op x y :. vZipWith op xs ys

toList :: Vec n a -> [a]
toList Nil       = []
toList (x :. xs) = x : toList xs

eToList :: Env (K a) sig -> [a]
eToList ENil        = []
eToList (K x :* xs) = x : eToList xs
