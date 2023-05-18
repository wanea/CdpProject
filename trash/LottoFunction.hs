
module LottoFunction where

import Data.Char (isNumber)
import Data.List (sort)
import LottoType (Ball (..), Ticket (..))

validateYorN:: String -> Bool
validateYorN "" = False
validateYorN str
          | isNumber x && x == '1' = True
          | otherwise   = False
          where (x:_) = str

-- check if input have all requirements
validateBall :: String -> Maybe Integer
validateBall "" = Nothing
validateBall str
          | isNum str && validRange = pure convInt
          | otherwise = Nothing
        where convInt = read str
              validRange = convInt > 0 && convInt < 50

isNum :: [Char] -> Bool
isNum ""     = True
isNum (x:xs) = isNumber x && isNum xs

addBallToTicket :: Ball Integer -> Ticket (Ball Integer) -> Ticket (Ball Integer)
addBallToTicket ball ticket
          | ballIsPresent ball ticket = ticket
          | otherwise = pure ball <> ticket

ballIsPresent :: Ball Integer -> Ticket (Ball Integer) -> Bool
ballIsPresent b t = b `elem` getTicket t

choiceToBall ::Maybe Integer -> Maybe (Ball Integer)
choiceToBall = maybe Nothing (pure . pure)

isFullTicket :: Ticket a -> Bool
isFullTicket ticket
          | len < 6 = False
          |otherwise = True
          where (Just len) = lengthOfTicket (Just ticket)

lengthOfTicket :: Maybe ( Ticket a) ->Maybe Int
lengthOfTicket mTicket = length <$> (getTicket <$> mTicket)

checkIfWin :: Ticket (Ball Integer) -> Ticket (Ball Integer) -> [Ball Integer]
checkIfWin (Ticket mylist) (Ticket winlist) =
  sortListBall [ x | x <- mylist , x `elem` winlist ]

sortedTicket ::  Ticket (Ball Integer) -> Ticket (Ball Integer)
sortedTicket ticket = Ticket $ Ball <$> sort (getBall <$> getTicket ticket)

sortListBall :: [Ball Integer] -> [Ball Integer]
sortListBall xs = Ball <$> sorted
      where sorted = sort  $ fmap getBall xs





