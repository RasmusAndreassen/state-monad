module StateRefined where
import Control.Monad
import System.Random (StdGen, Random (randomR, random), newStdGen)
import Control.Applicative (liftA3)

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

data TurnstileInput = Coin | Push
  deriving (Eq, Show)

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

newtype State s a = State {runState :: s -> (a, s)}

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
  fmap f sa = state $ \s ->
    let (a, s') = runState sa s
    in (f a, s')

instance Applicative (State s) where
  pure x = state (x,)
  sf <*> sa = state $ \s ->
    let (f, s') = runState sf s
        (a, s'') = runState sa s'
     in (f a, s'')

instance Monad (State s) where
  sa >>= opS = state $ \s0 ->
    let fa = runState sa
        (a, s1) = fa s0
        fb = runState $ opS a
        (b, s2) = fb s1
     in (b, s2)

turn :: TurnstileInput -> State TurnstileState TurnstileOutput
turn = state . turn'
  where
    turn' Coin _ = (Thank, Unlocked)
    turn' Push Unlocked = (Open, Locked)
    turn' Push Locked = (Tut, Locked)

put :: s -> State s ()
put newState = state $ const ((), newState)

get :: State s s
get = state $ \s -> (s, s)

(>.) :: (a -> b -> c) -> (c -> c') -> a -> b -> c'
o >. f = o |> (|> f)

evalState :: State s a -> s -> a
evalState = runState >. fst

execState :: State s a -> s -> s
execState = runState >. snd

getsThrough :: TurnstileInput -> State TurnstileState Bool
getsThrough input = do
  output <- turn input
  return $ output == Open

countOpens :: [TurnstileInput] -> State TurnstileState Int
countOpens = foldM incrementOnOpen 0
  where
    incrementOnOpen n i = do
      g <- getsThrough i
      return $ if g then n + 1 else n

modify :: (s -> s) -> State s ()
modify trans = state $ \s -> ((), trans s)

gets :: (s -> a) -> State s a
gets f = state $ \s -> (f s, s)

regularPerson, distractedPerson, hastyPerson :: State TurnstileState [TurnstileOutput]
regularPerson = mapM turn [Coin, Push]
distractedPerson = mapM turn [Coin]
hastyPerson = do
  o1 <- turn Push
  if o1 == Open
    then return $ return o1
    else do
      rest <- mapM turn [Coin, Push]
      return $ o1 : rest

tuesday :: State TurnstileState [TurnstileOutput]
tuesday =
  join
    <$> sequence
      [ regularPerson,
        hastyPerson,
        distractedPerson,
        hastyPerson
      ]

sequenceUntil :: (Traversable t, Monad m) => (a -> Bool) -> t (m a) -> m [a]
sequenceUntil predicate = foldM maybeDo []
  where
    maybeDo [] operation = do
      a <- operation
      return [a]
    maybeDo as operation = do
      let a = last as
       in if predicate a
            then return as
            else do
              a' <- operation
              return $ as ++ [a']

rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)

rollPair :: State StdGen (Int, Int)
rollPair = liftA2 (,) rollDie rollDie

rollDieDoubled :: State StdGen Int
rollDieDoubled = (*2) <$> rollDie

rollTwoSummed :: State StdGen Int
rollTwoSummed = liftA2 (+) rollDie rollDie

luckyDoubleS :: State StdGen Int
luckyDoubleS = do
  r1 <- rollDie
  if r1 == 6
    then do
      r2 <- rollDie 
      return (r1 + r2)
    else
      return r1

-- happyDouble :: State StdGen Int
-- happyDouble = do
--   r1 <- rollDie 
--   r2 <- rollDie 
--   return $ (if r1 == 6 then
--     (2*)else id ) (r1 + r2)

happyDouble :: State StdGen Int
happyDouble = op <$> rollDie <*> rollDie 
  where
    op 6 = (2 *) . (6 +)
    op m = (m +)

getRandom :: Random a => State StdGen a
getRandom = state random

someTypes :: State StdGen (Int, Float, Char)
someTypes = liftA3 (,,) getRandom getRandom getRandom

allTypes :: State StdGen (Int, Float, Char, Integer, Double, Bool, Int)
allTypes = (,,,,,,) <$> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom

randomElm :: [a] -> State StdGen a
randomElm as = do
  s <- get
  let n = length as - 1
      (i, s') = randomR (0, n) s
   in do put s'
         return $ as!!i

getRandomPair :: IO (Int, Int)
getRandomPair = do
  s <- newStdGen
  return $ evalState rollPair s


