import Data.List
import Expr
import Ops
import Util


nonCommutativeSplits
    :: [Integer]
    -> [([Integer], [Integer])]
nonCommutativeSplits xs =
    map (split xs) [1..2^length xs - 2]

commutativeSplits
    :: [Integer]
    -> [([Integer], [Integer])]
commutativeSplits xs =
    map (split xs) [1..2^(length xs - 1) - 1]

makeExprs'
    :: [Integer]
    -> ([Integer] -> [([Integer], [Integer])])
    -> [BinOp]
    -> [Expr]
makeExprs' xs splits ops = let
    exprPairs :: [Integer] -> [(Expr, Expr)]
    exprPairs xs' = [
        (lExpr, rExpr) | (left, right) <- splits xs',
                         lExpr         <- makeExprs left,
                         rExpr         <- makeExprs right]

    binExprs :: [(Expr, Expr)] -> [Expr]
    binExprs exprPairs' = [
        BinExpr lExpr binOp rExpr | (lExpr, rExpr) <- exprPairs',
                                    binOp          <- ops]
    in
        [UniExpr uniOp binExpr | uniOp   <- uniOps,
                                 binExpr <- binExprs (exprPairs xs)]

makeExprs
    :: [Integer]
    -> [Expr]
makeExprs [] =
    []
makeExprs [a] = [UniExpr uniOp (Term a) | uniOp <- uniOps]
makeExprs xs =
    makeExprs' xs    commutativeSplits    commutativeOps ++
    makeExprs' xs nonCommutativeSplits nonCommutativeOps

make
    :: ([Integer], Integer)
    -> Maybe Expr
make (xs, n) = find (\expr -> eval expr == (Just $ fromIntegral n)) $ makeExprs xs

makeAll
    :: [([Integer], Integer)]
    -> [Maybe Expr]
makeAll xns = fastMap make [(sort xs, n) | (xs, n) <- xns]

main = do
    let intLists = map (\n -> decTo 10 n 4) [0..9999]
    let exprs = makeAll $ zip intLists (repeat 10)
    let hasSol = sum [1 | Just _ <- exprs]
    sequence_ $ do
        (n, expr) <- zip intLists exprs
        let exprStr = case expr of Nothing  -> "No solution."
                                   (Just e) -> show $ rewrite e
        return $ putStrLn $ (show =<< n) ++ "    " ++ exprStr
    putStrLn ""
    putStrLn $ show hasSol ++ " of 10000 carriage numbers from 0000 to 9999 make 10."
