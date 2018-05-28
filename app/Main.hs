module Main where

import Lib

import Data.Char
import Data.List

alphabet :: [Char]
alphabet = (++) ['a'..'z'] [' ']

cipherSplit :: Int -> [Char]
cipherSplit shift = (++) (snd (splitAt shift alphabet)) (fst (splitAt shift alphabet))

returnChar :: Maybe Int -> Char
returnChar f = case f of
    Just f -> (!!) alphabet f
    Nothing -> error("Error in returnChar function.")

matchCeasar :: Char -> Int -> Char
matchCeasar char x = returnChar $ elemIndex char $ cipherSplit x

ceasar :: [Char] -> Int -> [Char]
ceasar [] shift = []
ceasar (x:xs) shift = matchCeasar x shift : ceasar xs shift

unceasar :: [Char] -> Int -> [Char]
unceasar msg shift = ceasar msg ((27 - shift) `mod` 27)

main :: IO ()
main = someFunc
