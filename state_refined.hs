import Control.Monad

(|>) = flip (.)

data TurnstileInput = Coin | Push
  deriving (Eq, Show)

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

turn :: TurnstileInput -> State TurnstileState TurnstileOutput
turn = state . turn
  where
    turn Coin _ = (Thank, Unlocked)
    turn Push Unlocked = (Open, Locked)
    turn Push Locked = (Tut, Locked)

put :: s -> State s ()
put newState = state $ const ((), newState)

get :: State s s
get = state $ \s -> (s, s)

o >. f = o |> (|> f)

evalState = runState >. fst

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
