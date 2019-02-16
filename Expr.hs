module Expr
( Expr (..)
, eval
, rewrite
) where

import Control.Applicative
import Data.Fixed
import Ops

data Expr
    = Term Integer
    | BinExpr Expr BinOp Expr
    | UniExpr UniOp Expr

instance Show Expr where
    show (Term a) = show a

    show (BinExpr a opA ex@(BinExpr b opB c)) =
        if   opA == opB && isAssociative opA
        then show a ++ show opA ++        show ex
        else show a ++ show opA ++ "(" ++ show ex ++ ")"
    show (BinExpr ex@(BinExpr a opA b) opB c) =
        if   opA == opB && isAssociative opA
        then        show ex ++        show opB ++ show c
        else "(" ++ show ex ++ ")" ++ show opB ++ show c
    show (BinExpr a Exp b) =
        if   (liftA2 (<)) (eval a) (Just 0) == Just True
        then "(" ++ show a ++ ")" ++ show Exp ++ show b
        else        show a        ++ show Exp ++ show b
    show (BinExpr a op b) = show a ++ show op ++ show b

    show (UniExpr Fac (Term a)) =        show a ++        show Fac
    show (UniExpr Fac a       ) = "(" ++ show a ++ ")" ++ show Fac

    show (UniExpr op (Term a)) = show op ++        show a
    show (UniExpr op a)        = show op ++ "(" ++ show a ++ ")"

rewrite
    :: Expr
    -> Expr
rewrite (Term a) = Term a
rewrite (BinExpr a op b) =
    let
        a' = rewrite a
        b' = rewrite b
    in
        case (a', op, b') of
            (UniExpr Neg a'', Mul, UniExpr Neg b'') -> BinExpr a'' Mul b''
            (UniExpr Neg a'', Mul, b''            ) -> UniExpr Neg (BinExpr a'' Mul b'')
            (a''            , Mul, UniExpr Neg b'') -> UniExpr Neg (BinExpr a'' Mul b'')

            (UniExpr Neg a'', Div, UniExpr Neg b'') -> BinExpr a'' Div b''
            (UniExpr Neg a'', Div, b''            ) -> UniExpr Neg (BinExpr a'' Div b'')
            (a''            , Div, UniExpr Neg b'') -> UniExpr Neg (BinExpr a'' Div b'')

            (UniExpr Neg a'', Add, UniExpr Neg b'') -> UniExpr Neg (BinExpr a'' Add b'')
            (UniExpr Neg a'', Add, b''            ) -> UniExpr Neg (BinExpr a'' Sub b'')
            (a''            , Add, UniExpr Neg b'') -> UniExpr Neg (BinExpr b'' Sub a'')

            (UniExpr Neg a'', Exp, b'') ->
                if   ((liftA2 mod') (eval b'') (Just 2) == Just 0)
                  && ((liftA2 (>=)) (eval b'') (Just 0)) == Just True
                then BinExpr a''               Exp b''
                else BinExpr (UniExpr Neg a'') Exp b''

            (_, _, _) -> BinExpr a' op b'

rewrite (UniExpr op a) =
    let
        a' = rewrite a
    in
        case (op, a') of
            (Id, a''             ) -> a''
            (Neg, Term 0         ) -> Term 0
            (Neg, UniExpr Neg a'') -> a''

            (_, _) -> UniExpr op a'

eval :: Expr -> Maybe Double
eval (Term a)         = Just $ fromIntegral a
eval (BinExpr a op b) = (binOpToFun op) (eval a) (eval b)
eval (UniExpr op a)   = (uniOpToFun op) (eval a)
