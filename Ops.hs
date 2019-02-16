module Ops
( BinOp (..)
, UniOp (..)
, binOpToFun
, binOps
, commutativeOps
, isAssociative
, isCommutative
, nonCommutativeOps
, uniOpToFun
, uniOps
) where

import Control.Applicative

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Exp
    deriving (Bounded, Enum, Eq)

data UniOp
    = Neg
    | Id
    | Flo
    | Cei
    | Fac
    deriving (Bounded, Enum, Eq)

instance Show BinOp where
    show Add = " + "
    show Sub = " - "
    show Mul = " × "
    show Div = " ÷ "
    show Exp = "^"

instance Show UniOp where
    show Id  = ""
    show Neg = "-"
    show Flo = "floor"
    show Cei = "ceil"
    show Fac = "!"

binOps :: [BinOp]
binOps = [(minBound :: BinOp)..(maxBound :: BinOp)]

uniOps :: [UniOp]
uniOps = [(minBound :: UniOp)..(maxBound :: UniOp)]

isAssociative :: BinOp -> Bool
isAssociative Add = True
isAssociative Mul = True
isAssociative _   = False

isCommutative :: BinOp -> Bool
isCommutative Add = True
isCommutative Mul = True
isCommutative _   = False

commutativeOps :: [BinOp]
commutativeOps = [binOp | binOp <- binOps, isCommutative binOp]

nonCommutativeOps :: [BinOp]
nonCommutativeOps = [binOp | binOp <- binOps, not $ isCommutative binOp]

binOpToFun
    :: BinOp
    -> (Maybe Double -> Maybe Double -> Maybe Double)
binOpToFun Add = liftA2 (+)
binOpToFun Sub = liftA2 (-)
binOpToFun Mul = liftA2 (*)
binOpToFun Div = div'
   where
      div' a (Just 0) = Nothing
      div' a b        = liftA2 (/) a b
binOpToFun Exp = liftA2 (**)

uniOpToFun
    :: UniOp
    -> (Maybe Double -> Maybe Double)
uniOpToFun Id  = liftA id
uniOpToFun Neg = liftA negate
uniOpToFun Flo = liftA (fromIntegral . floor)
uniOpToFun Cei = liftA (fromIntegral . ceiling)
uniOpToFun Fac = fact
    where
        fact (Just n) | n >= 0 && n < 10 = Just $ product [1..n]
                      | otherwise        = Nothing
        fact Nothing  = Nothing
