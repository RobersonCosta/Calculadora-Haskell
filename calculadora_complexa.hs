
applyOperator a b op x y
 | op == "+" = calc (a+x) (b+y)
 | op == "-" = calc (a-x) (b-y)
 | op == "*" = calc ((a * x) - (b * y)) ((a * y) + (b * x))
 | op == "/" && x == 0 && y == 0 = do
   putStrLn "Nao pode dividir por zero!!!"
   calc a b
 | otherwise = calc (((a * x) + (b * y))/(x^2 + y^2)) (((b * x) - (a * y))/(x^2 + y^2))

operate a b op
 | op == "limpar" = calc 0 0
 | op == "*" || op == "/" || op == "+" || op == "-" = do
   putStrLn "Indique a parte real:"
   x <- getLine
   putStrLn "Indique a parte imaginaria:"
   y <- getLine
   applyOperator a b op (read x :: Float) (read y :: Float)
 | otherwise = do
   putStrLn "Operacao desconhecida!!!"
   calc a b

calc a b = do
 putStrLn $ "Valor " ++ (show a) ++ " + " ++ (show b) ++ " i"
 putStrLn "Selecione operacao:"
 op <- getLine
 if op == "sair" then do
  putStrLn "Tchau!"
 else
  operate a b op

main = calc 0 0
