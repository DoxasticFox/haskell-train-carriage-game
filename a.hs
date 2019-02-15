-- TODO: Split into modules
import           Control.Applicative
import           Control.Parallel.Strategies
import           Data.Fixed
import           Data.List
import qualified Data.Map.Lazy as M

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Exp
    deriving (Enum, Bounded)

data UniOp
    = Neg
    | Id
    | Flo
    | Cei
    | Fac
    deriving (Enum, Bounded)

data Expr
    = Term Integer
    | BinExpr Expr BinOp Expr
    | UniExpr UniOp Expr

-- TODO: Simplify expressions?
instance Show Expr where
    show (Term a) = show a
    show (BinExpr a op b) = "(" ++ show a ++ show op ++ show b ++ ")"
    show (UniExpr Id a) = show a
    show (UniExpr Neg (Term 0)) = show 0
    show (UniExpr Fac a) = show a ++ show Fac
    show (UniExpr op a) = "(" ++ show op ++ show a ++ ")"

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
            (Id, Term a''        ) -> Term a''
            (Neg, UniExpr Neg a'') -> a''
            (Id , UniExpr Neg a'') -> UniExpr Neg a''
            (Neg, UniExpr Id  a'') -> UniExpr Neg a''

            (_, _) -> UniExpr op a'


binOps
    :: [BinOp]
binOps = [(minBound :: BinOp)..(maxBound :: BinOp)]

uniOps
    :: [UniOp]
uniOps = [(minBound :: UniOp)..(maxBound :: UniOp)]

isCommutative
    :: BinOp
    -> Bool
isCommutative Add = True
isCommutative Mul = True
isCommutative _   = False

commutativeOps
    :: [BinOp]
commutativeOps =
    [binOp | binOp <- binOps, isCommutative binOp]

nonCommutativeOps
    :: [BinOp]
nonCommutativeOps =
    [binOp | binOp <- binOps, not $ isCommutative binOp]

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
        fact (Just n) | n < 10    = Just $ product [1..n]
                      | otherwise = Nothing
        fact Nothing  = Nothing

decTo
    :: Integer
    -> Integer
    -> Integer
    -> [Integer]
decTo base n width =
    reverse $ decTo' n width
    where
        decTo' n width | n <= 0    = replicate (fromIntegral width) 0
                       | otherwise = n `rem` base : decTo' (n `quot` base) (width - 1)

split
    :: [Integer]
    -> Integer
    -> ([Integer], [Integer])
split xs n =
    let
        mask  = decTo 2 n (toInteger $ length xs)
        left  = [x | (x, m) <- zip xs mask, m == 1]
        right = [x | (x, m) <- zip xs mask, m /= 1]
    in
        (left, right)

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

eval :: Expr -> Maybe Double
eval (Term a) =
    Just $ fromIntegral a
eval (BinExpr a op b) =
    (binOpToFun op) (eval a) (eval b)
eval (UniExpr op a) =
    (uniOpToFun op) (eval a)

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

-- | Behaves like `map` but incorporates memoisation and parallelisation.
-- | The performance improvement over `map` is a little disappointing, but it's
-- | still better than nothing. It's possible that if someone who knew what they
-- | were doing took a shot at writing this, their version would take half the
-- | time to run.
fastMap
    :: Ord a
    => (a -> b)
    -> [a]
    -> [b]
fastMap f xs = runEval $ fastMap' M.empty f xs
    where
        fastMap'
            :: Ord a
            => M.Map a b
            -> (a -> b)
            -> [a]
            -> Eval [b]
        fastMap' cache f [] = return []
        fastMap' cache f (x:xs) = do
            v <- rpar $ M.findWithDefault (f x) x cache
            vs <- fastMap' (M.insert x v cache) f xs
            return $ v:vs

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
    sequence $ do
        (n, expr) <- zip intLists exprs
        let exprStr = case expr of Nothing  -> "No solution."
                                   (Just e) -> show $ rewrite e
        return $ putStrLn $ (show =<< n) ++ "    " ++ exprStr
    putStrLn ""
    putStrLn $ (show hasSol) ++ " of 10000 carriage numbers from 0000 to 9999 make 10."
