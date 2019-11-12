import Data.List
import System.IO
import Data.Maybe
import Data.List.Split
import Data.List (inits, tails)


pp1 = ""

gg10 = [[]] -- > VALIDO
gg11 = [[["alfa", "1", "2"]]] -- > INVALIDO
------------
pp2 = "alfa"

gg21 = [[]] -- > INVALIDO
gg22 = [[["alfa", "1", "2"]]] -- > VALIDO
gg23 = [[["beta", "1", "2"]]] -- > INVALIDO
gg24 = [[["alfa", "1", "2"], ["alfa", "2", "3"]]] -- > INVALIDO
gg25 = [[["alfa", "1", "2"], ["alfa", "2", "3"]], [["alfa", "1", "4"]]] -- > VALIDO
----------------------------------------------------------------------------------

pp3 = "alfa;beta"

gg31 = [[["alfa", "1", "2"]]] -- > INVALIDO
gg32 = [[["alfa", "1", "2"], ["beta", "2", "3"]]] -- > VALIDO
-----------------------------------------------------------

pp4 = "alfa;U(beta)(gama)"

gg41 = [[["alfa", "1", "2"], ["beta", "2", "3"]]] -- > VALIDO
gg42 = [[["alfa", "1", "2"], ["gama", "2", "3"]]] -- > VALIDO
gg43 = [[["alfa", "1", "2"], ["beta", "2", "3"]], [["alfa", "1", "4"], ["gama", "4", "5"]]] -- > VALIDO
-------------------------------------------------------------------------------------------------------

pp5 = "alfa;*(beta)"

gg51 = [[["alfa", "1", "2"]]] -- > VALIDO
gg52 = [[["alfa", "1", "2"], ["beta", "2", "3"], ["beta", "3", "4"]]] -- > VALIDO
gg53 = [[["alfa", "1", "2"], ["beta", "2", "3"], ["beta", "3", "4"], ["beta", "4", "5"]]] -- > VALIDO
gg54 = [[["alfa", "1", "2"],  ["omega", "2", "3"], ["beta", "3", "4"]]] -- > INVALIDO
--------------------------------------------------------------------------------------------------------

pp6 = "U(*(*(alfa);beta;U(psi)(fi);*(U(alef)(delta;teta))))(alfa)"

gg61 = [[]] -- > VALIDO
gg62 = [[["alfa", "1", "2"]]] -- > VALIDO
gg63 = [[["alfa", "1", "2"], ["beta", "2", "3"], ["psi", "3", "4"]]] -- > VALIDO
gg64 = [[["alfa", "1", "2"], ["beta", "2", "3"], ["psi", "3", "4"], ["alef", "4", "5"], ["alef", "5", "6"]]] -- > VALIDO
gg65 = [[["alfa", "1", "2"], ["beta", "2", "3"], ["psi", "3", "4"], ["alef", "4", "5"], ["delta", "5", "6"], ["teta", "6", "7"], ["delta", "7", "8"], ["teta", "8", "9"]]] -- > VALIDO
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pp7 = "U(U(alfa)(beta))(U(gama)(teta))"

gg71 = [[]] -- > INVALIDO
gg72 = [[["alfa", "1", "2"]]] -- > VALIDO
gg73 = [[["alfa", "1", "2"], ["beta", "2", "3"]]] -- > INVALIDO
-------------------------------------------------------------

pp8 = "*(U(U(alfa)(beta))(U(gama)(teta)))"

gg81 = [[]] -- > VALIDO
gg82 = [[["alfa", "1", "2"], ["alfa", "2", "3"], ["beta", "3", "4"], ["teta", "4", "5"]]]  -- > VALIDO


parseaParentesesEscND :: String -> [String]
parseaParentesesEscND str = vai 0 "" 0 str
    where
        vai :: Int -> String -> Int -> String -> [String]
        vai _ acc 2 [] = []
        vai _ acc 2 s = [s]
        vai _ acc _ [] = []
        vai balanco acc vezes (x:xs) = case x of
            '(' -> vai (balanco + 1) (acc ++ [x]) (vezes) (xs)
            ')' -> let novoBalanco = balanco - 1 in
                if novoBalanco == 0
                    then (acc ++ [x]) : vai 0 "" (vezes + 1) xs
                    else vai novoBalanco (acc ++ [x]) vezes xs
            _ -> vai (balanco) (acc ++ [x]) vezes xs

parseaParentesesFecho :: String -> [String]
parseaParentesesFecho str = vai 0 "" 0 str
    where
        vai :: Int -> String -> Int -> String -> [String]
        vai _ acc 1 [] = []
        vai _ acc 1 s = [s]
        vai _ acc _ [] = []
        vai balanco acc vezes (x:xs) = case x of
            '(' -> vai (balanco + 1) (acc ++ [x]) (vezes) (xs)
            ')' -> let novoBalanco = balanco - 1 in
                if novoBalanco == 0
                    then (acc ++ [x]) : vai 0 "" (vezes + 1) xs
                    else vai novoBalanco (acc ++ [x]) vezes xs
            _ -> vai (balanco) (acc ++ [x]) vezes xs

headAlt :: [a] -> [a]
headAlt [] = []
headAlt (a:as) = [a]

intercalaLisEmStr :: [String] -> String
intercalaLisEmStr a = intercalate ";" a

formataEscND :: [String] -> [String]
formataEscND [] = []
formataEscND a
    | length a >= 3 = [removeParenteses (head a) ++ head (tail (tail a))]  ++ [removeParenteses (head (tail a)) ++ head (tail (tail a))]
    | otherwise = [removeParenteses (head a)]  ++ [removeParenteses (head (tail a))]

formataFecho :: [String] -> [String]
formataFecho [] = []
formataFecho a
    | length a >= 2 = [removeParenteses (head a)] ++ tail a
    | otherwise = [removeParenteses (head a)]
 
escND :: [String] -> [[String]] -> (Bool, [String])
escND p g
    | verifica (head p) (g) == (True, [""]) = verifica (head p) (g)
    | verifica (head (tail p)) (g) == (True, [""]) = verifica (head (tail p)) (g)
    | otherwise = verifica (head p) (g)

removeParenteses :: String -> String
removeParenteses a
    | headAlt a == "(" =  reverse(tail(reverse(tail a)))
    | otherwise = a

contaPontoEVirgula :: String -> Int
contaPontoEVirgula "" = 1
contaPontoEVirgula a
    | headAlt a == ";" = 1 + contaPontoEVirgula (tail a)
    | otherwise = contaPontoEVirgula (tail a)

multiplica :: Int -> [String] -> String
multiplica 0 pfch
    | length pfch >= 2 = tail(head (tail pfch))
    | otherwise = ""
multiplica n pfch
    | n == 1 && length pfch < 2 = (head pfch) ++ (multiplica (n - 1) pfch)
    | otherwise = (head pfch) ++ ";" ++ (multiplica (n - 1) pfch)

fecho :: Int -> [String] -> [[String]] -> (Bool, [String])
fecho n pfch g
    | n == (0) = verifica (multiplica (0) (pfch)) (g)
    | verifica (multiplica (n) (pfch)) g == (True, [""]) = verifica (multiplica (n) (pfch)) g
    | otherwise = fecho (n - 1) (pfch) (g)

aaa :: [[String]] -> [[String]]
aaa [] = [["", "", ""]]
aaa a = a

ccc :: [[String]] -> [String]
ccc [] = ["", "", ""]
ccc c = head c

verifica :: String -> [[String]] -> (Bool, [String])
verifica "" [] = (True, [""])
verifica "" g =  (False, (head g))
verifica p g
    | headAlt p == "*" = fecho (length g) (formataFecho(parseaParentesesFecho (tail p))) g
    | headAlt p == "U" = escND (formataEscND (parseaParentesesEscND (tail p))) (g) 
    | head (splitOn ";" p) == head (head (aaa g)) = verifica (intercalaLisEmStr(tail (splitOn ";" p))) (tail g)
    | otherwise = (False, (ccc g))

get :: (Bool, [String]) -> Bool
get (a, b) = a

sumTuple :: (Int) -> (Int)
sumTuple (a) = (a+1)
    
verificaFora :: (Int) -> Int -> String -> [[[String]]] -> [((Int), (Bool, [String]))]
verificaFora _ 0 p [] = []
verificaFora i 1 p [] = []
verificaFora i _ p gL = [(i, verifica (p) (head gL))] ++ verificaFora (i+1) (1) p (tail gL)

pegaBool :: ((Int), (Bool, [String])) -> Bool
pegaBool ((i), (b, s)) = b

pegaInt :: ((Int), (Bool, [String])) -> String
pegaInt ((i), (b, s)) = show i

pegaStr :: ((Int), (Bool, [String])) -> [String]
pegaStr ((i), (b, s)) = s

true :: [((Int), (Bool, [String]))] -> Int
true [] = 0
true a
    | pegaBool (head a) == True = 1
    | otherwise = 0 + true (tail a)

printa :: Int -> [((Int), (Bool, [String]))] -> String
printa _ [((i), (b ,["", "", ""]))] = "O grafo nao eh valido para o programa pois esta vazio ou se esgotou antes do fim das transicoes do pograma pdl"
printa _ [] = ""
printa n w
    | n == 1 && pegaBool (head w) == True = "O grafo eh valido para o caminho numero " ++ pegaInt (head w)
    | n == 1 && pegaBool (head w) == False = printa (1) (tail w)
    | otherwise = "O grafo nao eh valido pois nao contempla o programa, devido a transicao " ++  head (pegaStr (head w)) ++ " entre os nohs " ++ head (tail (pegaStr (head w))) ++ " e " ++ head (tail (tail (pegaStr (head w)))) ++ " do caminho " ++ pegaInt (head w) ++ ". " ++ printa (0) (tail w)


main :: IO ()
main = do
    putStrLn "----------"
    putStrLn "Exemplo 1"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp1)
    putStrLn ("grafo: " ++ sgg10)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp1 gg10)) (verificaFora 0 0 pp1 gg10))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 2"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp1)
    putStrLn ("grafo: " ++ sgg11)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp1 gg11)) (verificaFora 0 0 pp1 gg11))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 3"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp2)
    putStrLn ("grafo: " ++ sgg21)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp2 gg21)) (verificaFora 0 0 pp2 gg21))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 4"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp2)
    putStrLn ("grafo: " ++ sgg22)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp2 gg22)) (verificaFora 0 0 pp2 gg22))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 5"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp2)
    putStrLn ("grafo: " ++ sgg23)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp2 gg23)) (verificaFora 0 0 pp2 gg23))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 6"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp2)
    putStrLn ("grafo: " ++ sgg24)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp2 gg24)) (verificaFora 0 0 pp2 gg24))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 7"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp2)
    putStrLn ("grafo: " ++ sgg25)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp2 gg25)) (verificaFora 0 0 pp2 gg25))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 8"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp3)
    putStrLn ("grafo: " ++ sgg31)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp3 gg31)) (verificaFora 0 0 pp3 gg31))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 9"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp3)
    putStrLn ("grafo: " ++ sgg32)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp3 gg32)) (verificaFora 0 0 pp3 gg32))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 10"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp4)
    putStrLn ("grafo: " ++ sgg41)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp4 gg41)) (verificaFora 0 0 pp4 gg41))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 11"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp4)
    putStrLn ("grafo: " ++ sgg42)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp4 gg42)) (verificaFora 0 0 pp4 gg42))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 12"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp4)
    putStrLn ("grafo: " ++ sgg43)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp4 gg43)) (verificaFora 0 0 pp4 gg43))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 13"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp5)
    putStrLn ("grafo: " ++ sgg51)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp5 gg51)) (verificaFora 0 0 pp5 gg51))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 14"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp5)
    putStrLn ("grafo: " ++ sgg52)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp5 gg52)) (verificaFora 0 0 pp5 gg52))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 15"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp5)
    putStrLn ("grafo: " ++ sgg53)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp5 gg53)) (verificaFora 0 0 pp5 gg53))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 16"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp5)
    putStrLn ("grafo: " ++ sgg54)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp5 gg54)) (verificaFora 0 0 pp5 gg54))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 17"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp6)
    putStrLn ("grafo: " ++ sgg61)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp6 gg61)) (verificaFora 0 0 pp6 gg61))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 18"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp6)
    putStrLn ("grafo: " ++ sgg62)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp6 gg62)) (verificaFora 0 0 pp6 gg62))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 19"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp6)
    putStrLn ("grafo: " ++ sgg63)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp6 gg63)) (verificaFora 0 0 pp6 gg63))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 20"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp6)
    putStrLn ("grafo: " ++ sgg64)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp6 gg64)) (verificaFora 0 0 pp6 gg64))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 21"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp6)
    putStrLn ("grafo: " ++ sgg65)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp6 gg65)) (verificaFora 0 0 pp6 gg65))
    putStrLn "\n"
    
    putStrLn "----------"
    putStrLn "Exemplo 22"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp7)
    putStrLn ("grafo: " ++ sgg71)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp7 gg71)) (verificaFora 0 0 pp7 gg71))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 23"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp7)
    putStrLn ("grafo: " ++ sgg72)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp7 gg72)) (verificaFora 0 0 pp7 gg72))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 24"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp7)
    putStrLn ("grafo: " ++ sgg73)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp7 gg73)) (verificaFora 0 0 pp7 gg73))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 25"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp8)
    putStrLn ("grafo: " ++ sgg81)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp8 gg81)) (verificaFora 0 0 pp8 gg81))
    putStrLn "\n"

    putStrLn "----------"
    putStrLn "Exemplo 26"
    putStrLn "----------"
    putStrLn ""
    putStrLn ("pdl: " ++ pp8)
    putStrLn ("grafo: " ++ sgg82)
    putStrLn ""
    putStrLn (printa (true (verificaFora 0 0 pp8 gg82)) (verificaFora 0 0 pp8 gg82))
    putStrLn "\n"
   

    
    

    



sgg10 = "[[]]" -- > VALIDO
sgg11 = "[[[alfa, 1, 2]]]" -- > INVALIDO
------------

sgg21 = "[[]]" -- > INVALIDO
sgg22 = "[[[alfa, 1, 2]]]" -- > VALIDO
sgg23 = "[[[beta, 1, 2]]]" -- > INVALIDO
sgg24 = "[[[alfa, 1, 2], [alfa, 2, 3]]]" -- > INVALIDO
sgg25 = "[[[alfa, 1, 2], [alfa, 2, 3]], [[alfa, 1, 4]]]" -- > VALIDO
----------------------------------------------------------------------------------


sgg31 = "[[[alfa, 1, 2]]]" -- > INVALIDO
sgg32 = "[[[alfa, 1, 2], [beta, 2, 3]]]" -- > VALIDO
-----------------------------------------------------------


sgg41 = "[[[alfa, 1, 2], [beta, 2, 3]]]" -- > VALIDO
sgg42 = "[[[alfa, 1, 2], [gama, 2, 3]]]" -- > VALIDO
sgg43 = "[[[alfa, 1, 2], [beta, 2, 3]], [[alfa, 1, 4], [gama, 4, 5]]]" -- > VALIDO
-------------------------------------------------------------------------------------------------------


sgg51 = "[[[alfa, 1, 2]]]"-- > VALIDO
sgg52 = "[[[alfa, 1, 2], [beta, 2, 3], [beta, 3, 4]]]" -- > VALIDO
sgg53 = "[[[alfa, 1, 2], [beta, 2, 3], [beta, 3, 4], [beta, 4, 5]]]" -- > VALIDO
sgg54 = "[[[alfa, 1, 2], [omega, 2, 3], [beta, 3, 4]]]" -- > INVALIDO
--------------------------------------------------------------------------------------------------------


sgg61 = "[[]]" -- > VALIDO
sgg62 = "[[[alfa, 1, 2]]]" -- > VALIDO
sgg63 = "[[[alfa, 1, 2], [beta, 2, 3], [psi, 3, 4]]]" -- > VALIDO
sgg64 = "[[[alfa, 1, 2], [beta, 2, 3], [psi, 3, 4], [alef, 4, 5], [alef, 5, 6]]]" -- > VALIDO
sgg65 = "[[[alfa, 1, 2], [beta, 2, 3], [psi, 3, 4], [alef, 4, 5], [delta, 5, 6], [teta, 6, 7], [delta, 7, 8], [teta, 8, 9]]]" -- > VALIDO

sgg71 = "[[]]" -- > INVALIDO
sgg72 = "[[[alfa, 1, 2]]]" -- > VALIDO
sgg73 = "[[[alfa, 1, 2], [beta, 2, 3]]]" -- > VALIDO
-------------------------------------------------------------

sgg81 = "[[]]" -- > VALIDO
sgg82 = "[[[alfa, 1, 2], [alfa, 2, 3], [beta, 3, 4], [teta, 4, 5]]]" -- > VALIDO