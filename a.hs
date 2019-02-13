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

decTo
    :: Int
    -> Int
    -> Int
    -> [Int]
decTo base n width =
    reverse $ decTo' n width
    where
        decTo' 0 width = replicate width 0
        decTo' n width = n `rem` base : decTo' (n `quot` base) (width - 1)

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

splits
    :: [a]
    -> [([a], [a])]
splits xs =
    map (split xs) [1..2^length xs - 2]

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
        [BinExpr lExpr'' binOp rExpr' |
            binOp     <-binOps,
            (lExprs, rExprs) <- zip lExprs rExprs,
            lExpr'   <- lExprs,
            rExpr'   <- rExprs,
            lExpr''  <- [UniExpr uniOp lExpr' | uniOp <- uniOps],
            rExpr''  <- [UniExpr uniOp rExpr' | uniOp <- uniOps]
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
