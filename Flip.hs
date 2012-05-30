module Flip where

import qualified Data.Map as Map

-- ref.
-- http://www.upsidedowntext.com/unicode
-- source code(javascript) from http://fliptitle.com

type Trans = (Char, Char)

lowerAlpha :: [Char]
lowerAlpha = ['a'..'z']
upperAlpha :: [Char]
upperAlpha = ['A'..'Z']
symbol :: [Char]
symbol = ['[',']','(',')','{','}','?','!','<','>','_',';',':','‿','⁅',',','.','\'','&']
number :: [Char]
number = ['0'..'9']

flippedLowerAlpha :: [Char]
flippedLowerAlpha = ['ɐ','q','ɔ','p','ǝ','ɟ','ƃ','ɥ','ı','ɾ','ʞ','l','ɯ','u','o','d','b','ɹ','s','ʇ','n','ʌ','ʍ','x','ʎ','z']
flippedUpperAlpha :: [Char]
flippedUpperAlpha = ['∀','B','Ɔ','D','Ǝ','Ⅎ','פ','H','I','ſ','K','˥','W','N','O','Ԁ','Ὸ','ᴚ','S','⊥','∩','Λ','M','X','⅄','Z']
flippedSymbol :: [Char]
flippedSymbol = [']','[',')','(','}','{','¿','¡','>','<','‾','؛',':','⁀','⁆','\'',',','˙','⅋']
flippedNumber :: [Char]
flippedNumber = ['0','Ɩ','ⵒ','Ɛ','߈','ϛ','9','ㄥ','8','6']

normal :: [Char]
normal = lowerAlpha ++ upperAlpha ++ symbol ++ number
flipped :: [Char]
flipped = flippedLowerAlpha ++ flippedUpperAlpha ++ flippedSymbol ++ flippedNumber

transTable :: Map.Map Char Char
transTable = Map.fromList $ zip normal flipped

main :: IO ()
main = interact (reverse . map trans)
  where
    trans c = case Map.lookup c transTable of
      Just c' -> c'
      Nothing -> c
