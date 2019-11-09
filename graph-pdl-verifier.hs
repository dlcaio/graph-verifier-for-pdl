import Data.List
import System.IO
import Data.Maybe
import Data.List.Split
import Data.List (inits, tails)

pdl = "beta;U(gama)(alfa;gama;bunta)"
pdl2 = "alfa;gama;bunta"

pdlFT = "*(alfa;beta);omega;*(beta;alfa)" -- -> (alfa;beta)

gFT = "alfa;beta;alfa;beta"

pdl3 = "U(gama;U(omega)(teta))(alfa;gama;bunta)"
pdl4 = "U(alfa;gama;bunta)(teta;U(gama;U(fi)(psi))(omega))"

graph = "alfa;gama;bunta"

pdlTAND = "U(U(alfa)(beta))(U(gama)(teta;omega))"
gTAND = "teta;omega"

pp = "alfa"
gg = "alfa"

pMonster = "*(U(alfa)(beta))"

ppp = "U(alfa)(beta);U(teta)(omega)"




splitChildren :: String -> [String]
splitChildren str = go 0 "" str
    where
        go :: Int -> String -> String -> [String]
        go _ acc [] = []
        go balance acc (x:xs) = case x of
            '(' -> go (balance + 1) (acc ++ [x]) xs
            ')' -> let newBalance = balance - 1 in
                if newBalance == 0
                    then (acc ++ [x]) : go 0 "" xs
                    else go newBalance (acc ++ [x]) xs
            _ -> go (balance) (acc ++ [x]) xs

splitFecho :: String -> [String]
splitFecho str = removeFirstSemiColonSF( goo 0 "" str)
    where
        goo :: Int -> String -> String -> [String]
        goo _ acc [] = []
        goo balance acc (x:xs) = case x of
            '(' -> goo (balance + 1) (acc ++ [x]) xs
            ')' -> let newBalance2 = balance - 1 in
                if newBalance2 == 0
                    then (acc ++ [x]) : [xs]
                    else goo newBalance2 (acc ++ [x]) xs
            _ -> goo (balance) (acc ++ [x]) xs

splitFechoOuter :: String -> [String]
splitFechoOuter a = [removeParenthesis (tail (head (splitFecho a)))] ++ tail (splitFecho a)

removeFirstSemiColonSF :: [String] -> [String]
removeFirstSemiColonSF (x:[""]) = [x]
removeFirstSemiColonSF (x:xs) = [x] ++ [tail (head xs)]

removeFirstSemiColon :: [String] -> [String]
removeFirstSemiColon a
    | hd (head a) == ";" = [tail (head a)]
    | otherwise = a

hd :: [a] -> [a]
hd [] = []
hd (a:as) = [a]

tl :: [String] -> String
tl a = intercalate ";" a


nonDetChoice :: [String] -> String -> Bool
nonDetChoice _ "" = False
nonDetChoice p g
    | verify (removeParenthesis (head p)) (g) == True = True -- -> verify ("(gama;U(omega)(teta))") (graph)
    | verify (removeParenthesis(head (tail p))) (g) == True = True
    | otherwise = False

removeParenthesis :: String -> String
removeParenthesis a
    | hd a == "(" =  reverse(tail(reverse(tail a)))
    | otherwise = a

--fecho ::  String -> [String] -> String -> Bool
--fecho fch rst g = verifyFecho (fch)(rst)(g)

--verifyFecho :: String -> [String] -> String -> Bool
--verifyFecho "" [] "" = True
--verifyFecho "" _ "" = False
--verifyFecho _ _ "" = False
--verifyFecho p g
--    | hd p == "U" = nonDetChoice (splitChildren (tail p)) (g) -- -> nonDetChoice ["(gama;U(omega)(teta))","(alfa;gama;bunta)"] graph
--    | hd p == "*" = fecho (splitChildren (tail p)) (g)
--    | head (splitOn ";" p) == head (splitOn ";" g) = verifyFecho (tl(tail (splitOn ";" p))) (tl(tail (splitOn ";" g))) 
--    | otherwise = False

--verifyFecho :: String -> String -> String -> Bool

countSemiColon :: String -> Int
countSemiColon "" = 0
countSemiColon a
    | hd a == ";" = 1 + countSemiColon (tail a)
    | otherwise = countSemiColon (tail a)


verify :: String -> String -> Bool
verify "" "" = True
verify "" _ = False
verify _ "" = False
verify p g
    | hd p == "U" = nonDetChoice (splitChildren (tail p)) (g) -- -> nonDetChoice ["(gama;U(omega)(teta))","(alfa;gama;bunta)"] graph
--    | hd p == "*" = fecho (countSemiColon g)(p) (g)
    | head (splitOn ";" p) == head (splitOn ";" g) = verify (tl(tail (splitOn ";" p))) (tl(tail (splitOn ";" g))) 
    | otherwise = False