{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeFamilies, PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Expr where

data UExpr where
  UNum :: Int -> UExpr
  UBool :: Bool -> UExpr
  UAdd :: UExpr -> UExpr -> UExpr
  UIsZero :: UExpr -> UExpr
  UIfThenElse :: UExpr -> UExpr -> UExpr -> UExpr
  deriving Show

data Expr (t :: EType) where
  Num        :: Int  -> Expr TInt
  Bool       :: Bool -> Expr TBool
  Add        :: Expr TInt -> Expr TInt -> Expr TInt
  IsZero     :: Expr TInt -> Expr TBool
  IfThenElse :: Expr TBool -> Expr a -> Expr a -> Expr a

deriving instance Show (Expr t)

data EType = TInt | TBool

data SEType (t :: EType) where
  STInt  :: SEType TInt
  STBool :: SEType TBool

deriving instance Show (SEType t)

data Equal :: k -> k -> * where
  Refl :: Equal t t

equalType :: SEType t1 -> SEType t2 -> Maybe (Equal t1 t2)
equalType STInt  STInt  = Just Refl
equalType STBool STBool = Just Refl
equalType _      _      = Nothing

data AExpr where
  AExpr :: SEType t -> Expr t -> AExpr

deriving instance Show AExpr

check :: UExpr -> Maybe AExpr
check (UNum i) = Just (AExpr STInt (Num i))
check (UBool b) = Just (AExpr STBool (Bool b))
check (UIsZero e) = do
  AExpr STInt e' <- check e
  return (AExpr STBool (IsZero e'))
check (UAdd e1 e2) = do
  AExpr STInt e1' <- check e1
  AExpr STInt e2' <- check e2
  return (AExpr STInt (Add e1' e2'))
check (UIfThenElse e1 e2 e3) = do
  AExpr STBool e1' <- check e1
  AExpr t2 e2' <- check e2
  AExpr t3 e3' <- check e3
  Refl <- equalType t2 t3
  return (AExpr t2 (IfThenElse e1' e2' e3'))

data Val = VInt Int | VBool Bool

{-
eval :: Expr -> Val
eval (Add e1 e2) =
  case (eval e1, eval e2) of
    (VInt v1, VInt v2) -> VInt (v1 + v2)
    _                  -> error "..."
-}

type family TypeOf (t :: EType) :: * where
  TypeOf TInt  = Int
  TypeOf TBool = Bool

eval :: Expr t -> TypeOf t
eval (Num n) = n
eval (Bool b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (IsZero e) = eval e == 0
eval (IfThenElse c t e) = if eval c then eval t else eval e
