import           Control.Applicative
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
    show Exp = " ^ "

instance Show UniOp where
    show Id  = ""
    show Neg = "-"
    show Flo = "floor"
    show Cei = "ceil"
    show Fac = "!"

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

memoise
    :: Ord a
    => (a -> b)
    -> [a]
    -> [b]
memoise = memoise' M.empty
    where
        memoise'
            :: Ord a
            => M.Map a b
            -> (a -> b)
            -> [a]
            -> [b]
        memoise' cache f [] = []
        memoise' cache f (x:xs) =
            let
                v = M.findWithDefault (f x) x cache
            in
                v : memoise' (M.insert x v cache) f xs

-- TODO: Parallelise
make
    :: ([Integer], Integer)
    -> Maybe Expr
make (xs, n) = find (\expr -> eval expr == (Just $ fromIntegral n)) $ makeExprs xs

makeAll
    :: [([Integer], Integer)]
    -> [Maybe Expr]
makeAll xns = (memoise make) [(sort xs, n) | (xs, n) <- xns]

main = do
    let intLists = map (\n -> decTo 10 n 3) [0..999]
    let exprs = makeAll $ zip intLists (repeat 10)
    let hasSol = sum [1 | Just _ <- exprs]
    sequence $ do
        (n, expr) <- zip intLists exprs
        let exprStr = case expr of Nothing  -> "No solution."
                                   (Just e) -> show e
        return $ putStrLn $ (show =<< n) ++ "    " ++ exprStr
    putStrLn ""
    putStrLn $ (show hasSol) ++ " of 10000 carriage numbers from 0000 to 9999 make 10."
