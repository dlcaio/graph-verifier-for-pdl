import Data.List
import System.IO
import Data.Maybe
import Data.List.Split
import Data.List (inits, tails)

pdl = "beta;U(gama)(alfa;gama;bunta)"
pdl2 = "alfa;gama;bunta"

pdlFT = "*(alfa;beta)" -- -> (alfa;beta)

gFT = "alfa;beta;alfa;beta"

pdl3 = "U(gama;U(omega)(teta))(alfa;gama;bunta)"
pdl4 = "U(alfa;gama;bunta)(teta;U(gama;U(fi)(psi))(omega))"

graph = "beta;alfa;gama;bunta"

pdlTAND = "U(U(alfa)(beta))(U(gama)(teta;omega))"
gTAND = "teta;omega"





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



verify :: String -> String -> Bool
verify "" "" = True
verify "" _ = False
verify _ "" = False
verify p g
    | hd p == "U" = nonDetChoice (splitChildren (tail p)) (g) -- -> nonDetChoice ["(gama;U(omega)(teta))","(alfa;gama;bunta)"] graph
    | head (splitOn ";" p) == head (splitOn ";" g) = verify (tl(tail (splitOn ";" p))) (tl(tail (splitOn ";" g))) 
    | otherwise = False