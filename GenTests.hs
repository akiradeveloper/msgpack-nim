import Test.QuickCheck

data Msg =
    MsgNil
	| MsgFalse
	| MsgTrue 
	| MsgFixArray [Msg]
	deriving (Show)

instance Arbitrary Msg where
  arbitrary = do
    n <- choose (1,4) :: Gen Int
    case n of
      1 -> return MsgNil
      2 -> return MsgFalse
      3 -> return MsgTrue
      4 -> do
        l <- choose (1,3) :: Gen Int
        list <- sequence $ [arbitrary :: Gen Msg | _ <- [1..l]]
        return $ MsgFixArray list

main = do
  msges <- sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..10]] :: IO [Msg]
  print msges
