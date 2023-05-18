
{-# LANGUAGE InstanceSigs #-}

module LottoType where
--all type i need to define

newtype Ball a = Ball {getBall:: a} deriving Eq
newtype Ticket a = Ticket {getTicket :: [a]}

instance Show a => Show (Ball a)  where
  show :: Ball a -> String
  show ball = "( Ball " ++ show (getBall ball) ++ " ) "

instance Show a => Show (Ticket a) where
  show :: Ticket a -> String
  show ticket = myTicket listBalls
    where listBalls = getTicket ticket
          myTicket []     = ""
          myTicket (x:xs) = show x ++ myTicket xs

instance Functor Ball where
  fmap :: (a->b) -> Ball a -> Ball b
  fmap f (Ball a) = Ball (f a)

instance Applicative Ball where
  pure :: a -> Ball a
  pure  = Ball
  (<*>):: Ball (a->b) -> Ball a -> Ball b
  f <*> ball = fmap (getBall f) ball

instance Functor Ticket where
  fmap :: (a->b) -> Ticket a -> Ticket b
  fmap f (Ticket a) = Ticket (f <$> a)

instance Applicative Ticket where
  pure :: a -> Ticket a
  pure x = Ticket [x]
  (<*>) :: Ticket (a->b) -> Ticket a -> Ticket b
  (Ticket f) <*> (Ticket x) = Ticket $ f <*> x

instance Semigroup (Ticket a) where
  (<>) :: Ticket a -> Ticket a -> Ticket a
  (Ticket x) <> (Ticket y) = Ticket (x<>y)

instance Foldable Ticket where
  foldr :: (a->b->b) -> b -> Ticket a  -> b
  foldr _ i (Ticket [])     = i
  foldr f i (Ticket (x:xs)) =  f x (foldr f i xs )

--- Useless now
instance Num a => Semigroup (Ball a) where
  (<>) :: Ball a -> Ball a -> Ball a
  (Ball x) <> (Ball y) = Ball (x+y)
