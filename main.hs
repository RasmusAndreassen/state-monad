
data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Unlocked = (Open, Locked)
push Locked = (Tut, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (o1, s1) = coin s0
      (o2, s2) = push s1
      (o3, s3) = push s2
      (o4, s4) = coin s3
      (o5, s5) = push s4
  in ([o1, o2, o3, o4, o5], s5)

regularPerson, distractedPerson, hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)

regularPerson s0 =
  let (o1, s1) = coin s0
      (o2, s2) = push s1
  in ([o1, o2], s2)

distractedPerson s0 =
  let (o1, s1) = coin s0
  in ([o1], s1)

hastyPerson s0 =
  let (o1, s1) = push s0
  in if o1 == Open
      then ([o1], s1)
      else let
      (o2, s2) = coin s1
      (o3, s3) = push s2
  in ([o1, o2, o3], s3)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 = let
  (o1, s1) = regularPerson s0
  (o2, s2) = regularPerson s1
  (o3, s3) = regularPerson s2
  (o4, s4) = regularPerson s3
  in (o1 ++ o2 ++ o3 ++ o4, s4)

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair isRegular s0 = 
  let (o1, s1) = if isRegular
                  then distractedPerson s0
                  else regularPerson s0
      (o2, s2) = push s1
  in (o2 == Open, s2)
