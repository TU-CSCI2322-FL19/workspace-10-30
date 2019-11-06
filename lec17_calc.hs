import Data.Char

inputStr = "+ 7 / 232 * 3 4"
input2= "+ * 2 3 * 3 4"
inputWords = words inputStr

-- data Token = MultT | MinusT | PlusT | DivT | NumT Integer deriving (Eq, Show)

data Token = OperT Op | NumT Integer deriving (Eq, Show)
data Op = Mult | Minus | Plus | Div deriving (Eq, Show)


--step 1
dangerLex :: String -> Token
dangerLex "+" = OperT Plus
dangerLex "-" = OperT Minus
dangerLex "*" = OperT Mult
dangerLex "/" = OperT Div
dangerLex str = NumT $ read str

--step 2
lexer :: String -> Maybe Token
lexer "+" = Just $ OperT Plus
lexer "-" = Just $ OperT Minus
lexer "*" = Just $ OperT Mult
lexer "/" = Just $ OperT Div
lexer ('-':str) = if all isDigit str  
                  then Just $ NumT $ read str
                  else Nothing
lexer str = if all isDigit str  
            then Just $ NumT $ read str
            else Nothing

(Just tokens) = sequence $ map lexer inputWords 
-- do not use fromJust or refutable patterns ever outside of a class activity

data AST = Leaf Integer | Node Op AST AST deriving (Eq, Show)

goalAST = Node Plus (Leaf 7) (Node Div (Leaf 232) (Node Mult (Leaf 3) (Leaf 4)))
tokenGoal2 = [OperT Plus, OperT Mult, NumT 2, NumT 3, OperT Mult, NumT 3, NumT 4]

--Step 2
dangerParse :: [Token] -> Maybe AST
dangerParse tokens = 
    let (tree, afterTree) =  aux tokens
    in if null afterTree
       then tree
       else error "Invalid input."

aux :: [Token] -> Maybe (AST, [Token])
aux [] = error "Invalid input."
aux (NumT x:ts) = (Leaf x, ts)
aux (OperT op:ts) = 
            let (lft, afterLeft) = aux ts
                (rgt, afterRight) = aux afterLeft
            in (Node op lft rgt, afterRight)

--Step 3
evalString :: String -> Maybe Integer
evalString = undefined



foo = let wlAlpha = 1:wlBeta
          wlBeta = 2:wlAlpha
      in wlAlpha
--Step 1
eval :: AST -> Integer
eval (Leaf x) = x
eval (Node Plus lft rgt) = (eval lft) + (eval rgt)
eval (Node Mult lft rgt) = (eval lft) * (eval rgt)
eval (Node Div lft rgt) = (eval lft) `div` (eval rgt)
eval (Node Minus lft rgt) = (eval lft) - (eval rgt)

eval2 :: AST -> Integer
eval2 (Leaf x) = x
eval2 (Node op lft rgt) = evalOp op (eval2 lft) (eval2 rgt)

evalOp :: Op -> Integer -> Integer -> Integer
evalOp Plus = (+)
evalOp Mult = (*)
evalOp Div = div
evalOp Minus = (-)

--Optional but encouraged Step 0
isOp :: Token -> Bool
isOp (NumT x) = False
isOp (OperT op) = True

numOps :: AST -> Integer
numOps (Leaf x) = 0
numOps (Node op lft rgt) = 1 + (numOps lft) + (numOps rgt)


