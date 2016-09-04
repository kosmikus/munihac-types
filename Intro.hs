{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Intro where

data Questions n = Questions (Vec n Question)

data Question =
    YesNo String Bool
  | Numeric String Int

data Answers n = Answers (Vec n Answer)

data Answer =
    AYesNo Bool
  | ANumeric Int

data Scoring n = Scoring (Vec n ScoreFunction)

data ScoreFunction =
    SYesNo Int Int
  | SNumeric (Int -> Int)

score :: Questions n -> Answers n -> Int
score (Questions qs) (Answers as) =
  let v = vZipWith
            (\ (Question _ correct) b -> if correct == b then 1 else 0)
            qs
            as
  in sum (toList v)

data QuestionType = QYesNo | QNumeric

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
