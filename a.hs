import Data.List
import Data.Char

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

instance Show Expr where
    show (Term t) = show t
    show (BinExpr a op b) = "(" ++ show a ++ show op ++ show b ++ ")"
    show (UniExpr Id a) = show a
    show (UniExpr Neg (Term 0.0)) = show 0.0
    show (UniExpr Fac ex) = show ex ++ show Fac
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

zeroPad
    :: [Int]
    -> Int
    -> [Int]
zeroPad xs width =
    let
        padLen = max 0 (width - length xs)
    in
        (replicate padLen 0) ++ xs

decTo'
    :: Int
    -> Int
    -> [Int]
decTo' base 0 = []
decTo' base n = decTo' base (n `quot` base) ++ [n `rem` base]

decTo
    :: Int
    -> Int
    -> Int
    -> [Int]
decTo base dec width =
    let bin = decTo' base dec
    in  zeroPad bin width

decToBin
    :: Int
    -> Int
    -> [Int]
decToBin dec width = decTo 2 dec width

split
    :: [a]
    -> Int
    -> ([a], [a])
split xs n =
    let
        mask  = decToBin n (length xs)
        left  = [x | (x, m) <- zip xs mask, m == 1]
        right = [x | (x, m) <- zip xs mask, m /= 1]
    in
        (left, right)

splits
    :: [a]
    -> [([a], [a])]
splits xs =
    map (split xs) [0..2^length xs - 2]

eval :: Expr -> Double
eval (Term a)
    = a
eval (BinExpr a op b)
    = (binOpToFun op) (eval a) (eval b)
eval (UniExpr op a)
    = (uniOpToFun op) (eval a)

makeAll
    :: [Double]
    -> [Expr]
makeAll [] = []
makeAll [a] = [Term a]
makeAll xs =
    let
        (left, right) = unzip $ splits xs
        (lExprs, rExprs) = (map makeAll left, map makeAll right)
        binOps = [(minBound :: BinOp)..]
        uniOps = [(minBound :: UniOp)..]
    in
        [BinExpr lExpr__ binOp rExpr_ |
            binOp     <-binOps,
            (lExprs, rExprs) <- zip lExprs rExprs,
            lExpr_   <- lExprs,
            rExpr_   <- rExprs,
            lExpr__  <- [UniExpr uniOp lExpr_ | uniOp <- uniOps],
            rExpr__  <- [UniExpr uniOp rExpr_ | uniOp <- uniOps]
            ]

make
    :: [Double]
    -> Double
    -> Maybe Expr
make xs n = find (\expr -> eval expr == n) $ makeAll xs

main = do
    let ns = [0..9999]
    let intLists = map (\n -> decTo 10 n 4) ns
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
