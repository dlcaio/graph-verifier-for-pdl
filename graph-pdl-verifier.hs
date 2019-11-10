import Data.List
import System.IO
import Data.Maybe
import Data.List.Split
import Data.List (inits, tails)

pdl = "beta;U(gama)(alfa;gama;bunta)"
pdl2 = "alfa;gama;bunta"

pdlFT = "*(alfa);beta;*(alfa)" -- -> (alfa;beta)

gFT = "alfa;alfa;beta"

pdl4 = "U(alfa;gama;bunta)(teta;U(gama;U(fi)(psi))(omega))"





pdl3 = "alfa;U(gama;U(omega)(teta))(alfa;gama;bunta);bunta;alfa"
graph = "alfa;alfa;gama;bunta;bunta;alfa;beta"
pdl3v2 = "U(gama;U(omega)(teta))(alfa;gama;bunta);bunta;alfa;U(alfa)(beta)"
graph2 = "alfa;gama;bunta;bunta;alfa;beta"


ppp = "U(alfa)(beta);beta"
ggg = "beta;beta"

pdlTAND = "fi;U(U(alfa)(beta))(U(gama)(teta;omega);psi);beta"--;U(yuyu)(hakusho)"
gTAND = "teta;omega"

gTAND2 = "fi;teta;omega;psi;beta"

pppp = "beta;U(omega;psi;U(alfa)(beta))(teta);psi;U(um)(dois)"

gggg = "beta;omega;psi;alfa;psi;um"

pppppp = "U(alfa)(beta)"
gggggg = "alfa"



splitChildren :: String -> [String]
splitChildren str = go 0 "" 0 str
    where
        go :: Int -> String -> Int -> String -> [String]
        go _ acc 2 [] = []
        go _ acc 2 s = [s]
        go _ acc _ [] = []
        go balance acc times (x:xs) = case x of
            '(' -> go (balance + 1) (acc ++ [x]) (times) (xs)
            ')' -> let newBalance = balance - 1 in
                if newBalance == 0
                    then (acc ++ [x]) : go 0 "" (times + 1) xs
                    else go newBalance (acc ++ [x]) times xs
            _ -> go (balance) (acc ++ [x]) times xs

splitChildren1 :: String -> [String]
splitChildren1 str = go 0 "" 0 str
    where
        go :: Int -> String -> Int -> String -> [String]
        go _ acc 1 [] = []
        go _ acc 1 s = [s]
        go _ acc _ [] = []
        go balance acc times (x:xs) = case x of
            '(' -> go (balance + 1) (acc ++ [x]) (times) (xs)
            ')' -> let newBalance = balance - 1 in
                if newBalance == 0
                    then (acc ++ [x]) : go 0 "" (times + 1) xs
                    else go newBalance (acc ++ [x]) times xs
            _ -> go (balance) (acc ++ [x]) times xs

hd :: [a] -> [a]
hd [] = []
hd (a:as) = [a]

tl :: [String] -> String
tl a = intercalate ";" a

concatena :: [String] -> [String]
concatena [] = []
concatena a
    | length a >= 3 = [removeParenthesis (head a) ++ head (tail (tail a))]  ++ [removeParenthesis (head (tail a)) ++ head (tail (tail a))]
    | otherwise = [removeParenthesis (head a)]  ++ [removeParenthesis (head (tail a))]

concatena1 :: [String] -> [String]
concatena1 [] = []
concatena1 a
    | length a >= 2 = [removeParenthesis (head a)] ++ tail a
    | otherwise = [removeParenthesis (head a)]
 
nonDetChoice :: [String] -> String -> Bool
nonDetChoice _ "" = False
nonDetChoice p g
    | verify (head p) (g) == True = True -- -> verify ("(gama;U(omega)(teta))") (graph)
    | verify (head (tail p)) (g) == True = True
    | otherwise = False

removeParenthesis :: String -> String
removeParenthesis a
    | hd a == "(" =  reverse(tail(reverse(tail a)))
    | otherwise = a

countSemiColonPlusOne :: String -> Int
countSemiColonPlusOne "" = 1
countSemiColonPlusOne a
    | hd a == ";" = 1 + countSemiColonPlusOne (tail a)
    | otherwise = countSemiColonPlusOne (tail a)

multiplica :: Int -> [String] -> String
multiplica 0 pfch
    | length pfch >= 2 = tail(head (tail pfch))
    | otherwise = ""
multiplica n pfch
    | n == 1 && length pfch < 2 = (head pfch) ++ (multiplica (n - 1) pfch)
    | otherwise = (head pfch) ++ ";" ++ (multiplica (n - 1) pfch)

fecho :: Int -> [String] -> String -> Bool
fecho (-1) pfch g = False
--fecho 0 pfch g = verify (multiplica (0) (pfch)) (g)
fecho n pfch g
    | verify (multiplica (n) (pfch)) g == True = True
    | otherwise = fecho (n - 1) (pfch) (g)

verify :: String -> String -> Bool
verify "" "" = True
verify "" _ = False
verify _ "" = False
verify p g
    | hd p == "*" = fecho (countSemiColonPlusOne g) (concatena1(splitChildren1 (tail p))) g
    | hd p == "U" = nonDetChoice (concatena (splitChildren (tail p))) (g) -- -> nonDetChoice ["(gama;U(omega)(teta))","(alfa;gama;bunta)"] graph
    | head (splitOn ";" p) == head (splitOn ";" g) = verify (tl(tail (splitOn ";" p))) (tl(tail (splitOn ";" g))) 
    | otherwise = False

-- concatena1(splitChildren1(tail pdlFT))   ->     ["U(alfa)(beta)"]