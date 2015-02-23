import Test.QuickCheck

data Msg =
    MsgNil
	| MsgFalse
	| MsgTrue 
	| MsgFixArray [Msg]
	deriving (Show)

instance Arbitrary Msg where
  arbitrary = do
    n <- choose (1,3) :: Gen Int
    case n of
      1 -> return MsgNil
      2 -> return MsgFalse
      3 -> return MsgTrue

main = do
  sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..10]]
