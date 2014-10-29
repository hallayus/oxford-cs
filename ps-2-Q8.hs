import Data.Char
import Data.List

song 0 = ""
song n 
  | n >= 10 = error "number of verses exceeded 10"
  | otherwise = song (n - 1) ++ "\n" ++ verse n

verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

line1 n = (menKeyPhrase n) ++ " went to mow" ++  "\n"
  
line2 n = "Went to mow a meadow" ++ "\n"
line3 n = let line3men 0 = ""
              line3men p = (menKeyPhrase p) ++ ", " ++ line3men (p - 1)            
          in init (init (line3men n)) ++ " and his dog" ++ "\n"
line4 n = line2 n 

menKeyPhrase n
  | n == 1 = "One man" 
  | otherwise = let convert num 
                      | num >= 10 = error "number of verses exceeded 10"
                      | otherwise = numbers !! (num - 1)
                in (convert n) ++ " men"

numbers = ["One","Two","Three","Four","Five","Six","Seven","Eight","Nine"]
