import Data.List
import System.IO
import Data.Maybe
import Data.List.Split
import Data.List (inits, tails)

pdl = "beta;U(gama)(alfa;gama;bunta)"
pdl2 = "alfa;gama;bunta"

pdlFT = "*(alfa;beta)" -- -> (alfa;beta)

gFT = "alfa;beta;alfa;beta"

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



verify :: String -> String -> Bool
verify "" "" = True
verify "" _ = False
verify _ "" = False
verify p g
    | hd p == "U" = nonDetChoice (concatena (splitChildren (tail p))) (g) -- -> nonDetChoice ["(gama;U(omega)(teta))","(alfa;gama;bunta)"] graph
    | head (splitOn ";" p) == head (splitOn ";" g) = verify (tl(tail (splitOn ";" p))) (tl(tail (splitOn ";" g))) 
    | otherwise = False