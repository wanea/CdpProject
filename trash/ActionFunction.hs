module ActionFunction where


import LottoFunction (addBallToTicket, isFullTicket, sortedTicket)
import System.Random (randomRIO)
import LottoType (Ball (Ball), Ticket (Ticket))
{-=============================================================================
=                          Printable Function
==============================================================================-}
welcome :: IO()
welcome = do
  putStrLn "-------------------------------------------------------"
  putStrLn "     You are Welcome in this small Lotto project      "
  putStrLn "-------------------------------------------------------"



printRandomTicket :: Ticket (Ball Integer) -> IO ()
printRandomTicket ticket = do
  putStrLn "------------------------------------------------"
  putStrLn $ "The Random ticket is  : " ++ show ticket


{-=============================================================================
=                               Generate Random ticket
==============================================================================-}
emptyTicket :: Ticket (Ball Integer)
emptyTicket = Ticket []


randChoice :: IO (Ball Integer)
randChoice = do
  num <- randomRIO (1,49)
  pure $ Ball num


addBallRand :: Ticket (Ball Integer) -> IO (Ticket (Ball Integer))
addBallRand ticket =  do
  if isFullTicket ticket
    then pure (sortedTicket ticket)
  else do
    aBall <- randChoice
    addBallRand $ addBallToTicket aBall ticket


randTicket  :: IO (Ticket (Ball Integer))
randTicket  = addBallRand emptyTicket


