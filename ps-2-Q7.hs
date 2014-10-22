{-|
a better solution probably involves using tables, but for the sake of
simplicity and sticking to more common haskell libraries, I make my own
list lookup function using tuples
-}

-- assume first element of pair is primary key

import Control.Applicative


lookUpNumeral :: [RNumeral] -> (RNumType,Int) -> Maybe Char
lookUpNumeral (x:xs) (t,p) = if (rType x) == t && (rPower x == p) then Just (rLetter x)
                    else lookUpNumeral xs (t,p)
lookUpNumeral [] _ = Nothing

digitConvert :: (Char, Int) -> Maybe String
digitConvert (digit,power)
    | digit == '1' = fmap (\char -> [char]) (lookUpNumeral numList (Unit,power))
    | digit == '2' = liftA2 (++) (digitConvert ('1',power)) (digitConvert ('1',power)) 
    | digit == '3' = liftA2 (++) (digitConvert ('2',power)) (digitConvert ('1',power))                
    | digit == '4' = liftA2 (++) (digitConvert ('1',power)) (digitConvert ('5',power))
    | digit == '5' = fmap (\char -> [char]) (lookUpNumeral numList (Five,power))
    | digit == '6' = liftA2 (++) (digitConvert ('5',power)) (digitConvert ('1',power))
    | digit == '7' = liftA2 (++) (digitConvert ('5',power)) (digitConvert ('2',power))
    | digit == '8' = liftA2 (++) (digitConvert ('5',power)) (digitConvert ('3',power))
    | digit == '9' = liftA2 (++) (digitConvert ('1',power)) (digitConvert ('1',power + 1))
    | digit == '0' = Just ""


data RNumType = Unit | Five deriving (Show, Eq)

data RNumeral = RNumeral { rType :: RNumType,
                            rPower :: Int,
                            rLetter :: Char
                          } deriving (Show, Eq)

numList = [
                  RNumeral Unit 0 'I',
                  RNumeral Five 0 'V',
                  RNumeral Unit 1 'X',
                  RNumeral Five 1 'L',
                  RNumeral Unit 2 'C',
                  RNumeral Five 2 'D',
                  RNumeral Unit 3 'M'
              ]

romanNumeral :: Int -> Maybe String
romanNumeral num = let numeralIterator (char:string) acc = liftA2 (++) (digitConvert (char,acc)) (numeralIterator string (acc-1))
                       numeralIterator [] _ = Just []
                       digits = show num
                   in numeralIterator digits ((length digits) - 1)

