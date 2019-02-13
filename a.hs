import Data.Char
import Data.List

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
    = Term Double
    | BinExpr Expr BinOp Expr
    | UniExpr UniOp Expr

-- TODO: Shouldn't render as doubles
instance Show Expr where
    show (Term a) = show a
    show (BinExpr a op b) = "(" ++ show a ++ show op ++ show b ++ ")"
    show (UniExpr Id a) = show a
    show (UniExpr Neg (Term 0.0)) = show 0.0
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
    -> (Double -> Double -> Double)
binOpToFun Add = (+)
binOpToFun Sub = (-)
binOpToFun Mul = (*)
binOpToFun Div = (/)  -- TODO: (0/0) ^ 0 != 1
binOpToFun Exp = (**) -- TODO: 0^0 != 1

uniOpToFun
    :: UniOp
    -> (Double -> Double)
uniOpToFun Id  = id
uniOpToFun Neg = negate
uniOpToFun Flo = fromIntegral . floor
uniOpToFun Cei = fromIntegral . ceiling
uniOpToFun Fac = fact
    where fact n | n < 10    = product [1..n]
                 | otherwise = 0/0

decTo
    :: Int
    -> Int
    -> Int
    -> [Int]
decTo base n width =
    reverse $ decTo' n width
    where
        decTo' n width | n <= 0    = replicate width 0
                       | otherwise = n `rem` base : decTo' (n `quot` base) (width - 1)

split
    :: [a]
    -> Int
    -> ([a], [a])
split xs n =
    let
        mask  = decTo 2 n (length xs)
        left  = [x | (x, m) <- zip xs mask, m == 1]
        right = [x | (x, m) <- zip xs mask, m /= 1]
    in
        (left, right)

nonCommutativeSplits
    :: [a]
    -> [([a], [a])]
nonCommutativeSplits xs =
    map (split xs) [1..2^length xs - 2]

commutativeSplits
    :: [a]
    -> [([a], [a])]
commutativeSplits xs =
    map (split xs) [1..2^(length xs - 1) - 1]

eval :: Expr -> Double
eval (Term a) =
    a
eval (BinExpr a op b) =
    (binOpToFun op) (eval a) (eval b)
eval (UniExpr op a) =
    (uniOpToFun op) (eval a)

makeAll'
    :: [Double]
    -> ([Double] -> [([Double], [Double])])
    -> [BinOp]
    -> [Expr]
makeAll' xs splits ops = let
    exprPairs = [
        (lExprs, rExprs) | (left, right) <- splits xs,
                           lExprs        <- makeAll left,
                           rExprs        <- makeAll right]

    binExprs = [
        BinExpr lExpr binOp rExpr | (lExpr, rExpr) <- exprPairs,
                                    binOp          <- ops]
    in
        [UniExpr uniOp binExpr | uniOp   <- uniOps,
                                 binExpr <- binExprs]

makeAll
    :: [Double]
    -> [Expr]
makeAll [] =
    []
makeAll [a] = [UniExpr uniOp (Term a) | uniOp <- uniOps]
makeAll xs =
    makeAll' xs    commutativeSplits    commutativeOps ++
    makeAll' xs nonCommutativeSplits nonCommutativeOps

-- TODO: Memoise
make
    :: [Double]
    -> Double
    -> Maybe Expr
make xs n = find (\expr -> eval expr == n) $ makeAll xs

main = do
    let ns = [0..100]
    let intLists = map (\n -> decTo 10 n 3) ns
    let nLists = map (map fromIntegral) intLists
    let exprs = map (\trainCarriageNumber -> make trainCarriageNumber 10) nLists
    let hasSol = sum [1 | (Just _) <- exprs]
    sequence $ do
        (n, expr) <- zip intLists exprs
        let exprStr = case expr of Nothing  -> "No solution."
                                   (Just e) -> show e
        return $ putStrLn $ (map intToDigit n) ++ "    " ++ exprStr
    putStrLn ""
    putStrLn $ (show hasSol) ++ " of 10000 carriage numbers from 0000 to 9999 make 10."
