import Control.Monad

(|>) = flip (.)

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

newtype State s a = State {runState :: s -> (a, s)}

state = State

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure x = state (x,)
  (<*>) = ap

instance Monad (State s) where
  sa >>= opS = state $ \s0 ->
    let fa = runState sa
        (a, s1) = fa s0
        fb = runState $ opS a
        (b, s2) = fb s1
     in (b, s2)

type Turnstile = State TurnstileState

coin, push :: Turnstile TurnstileOutput

monday =
  sequence
    [ coin,
      push,
      push,
      coin,
      push
    ]

regularPerson, distractedPerson, hastyPerson :: Turnstile [TurnstileOutput]
regularPerson = sequence [coin, push]
distractedPerson = sequence [coin]
hastyPerson = do
  o1 <- push
  if o1 == Tut
    then do
      os <- regularPerson
      return $ o1 : os
    else
      return [o1]

o >. f = o |> (|> f)

evalState :: State s a -> s -> a
evalState = runState >. fst

execState :: State s a -> s -> s
execState = runState >. snd

testTurnstile :: Turnstile Bool
testTurnstile = do
  put Locked
  check1 <- push
  put Unlocked
  check2 <- push
  put Locked
  return $ check1 == Tut && check2 == Open

put :: s -> State s ()
put newState = state $ const ((), newState)

get :: State s s
get = state $ \s -> (s, s)

coin = do
  put Unlocked
  return Thank

push = do
  s <- get
  put Locked
  return $ case s of
    Locked -> Tut
    Unlocked -> Open
