inputStr = "+ 7 - 232 * 3 4"
inputWords = words inputStr

-- data Token = MultT | MinusT | PlusT | DivT | NumT Integer deriving (Eq, Show)

data Token = OperT Op | NumT Integer deriving (Eq, Show)
data Op = Mult | Minus | Plus | Div deriving (Eq, Show)

tokenGoal = [OperT Plus, NumT 7, OperT Minus, NumT 232, OperT Mult, NumT 3, NumT 4]

--step 1
lexer :: String -> Token

--step 2
lexer :: String -> Maybe Token

tokenInput = map lexer inputWords
