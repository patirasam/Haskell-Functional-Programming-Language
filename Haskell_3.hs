

data Expression = EInt Integer
			| Add Expression Expression
			| Divide Expression Expression
			deriving (Show)


evaluate :: Expression -> Either String Integer
evaluate (EInt x) = return x
evaluate (Add e1 e2) = do
	i <- evaluate e1
	j <- evaluate e2
	return $ i + j
evaluate (Divide e1 e2) = do
	i <- evaluate e1
	j <- evaluate e2
	if j == 0
		then Left " Cannot Divide by zero"
	else 
		return $ i `div` j


evalAndPrint e = case evaluate e of 
				Left s -> error $ "error" ++ s
				Right i -> print $ "Result: " ++ show i

main = do 
	evalAndPrint (Add (EInt 5) (Add (EInt 2) (EInt 4)))
	evalAndPrint (Divide (EInt 3) (EInt 2))
	evalAndPrint (Divide (EInt 1) (EInt 0))

