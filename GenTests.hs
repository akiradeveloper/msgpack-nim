import Data.List
import Test.QuickCheck
import Control.Monad

data Int7Bit = Int7Bit Int
data Int5Bit = Int5Bit Int
data Uint16 = Uint16 Int

data Msg =
    MsgNil
  | MsgFalse
  | MsgTrue
  | MsgFixArray [Msg]
  | MsgPFixNum Int7Bit
  | MsgNFixNum Int5Bit
  | MsgU16 Uint16

msgShow MsgNil = "Nil()"
msgShow MsgFalse = "False()"
msgShow MsgTrue = "True()"
msgShow (MsgFixArray xs) = "FixArray(@[" ++ (intercalate "," $ map msgShow xs) ++ "])"
msgShow (MsgPFixNum (Int7Bit n)) = "PFixNum(" ++ show n ++ ")"
msgShow (MsgNFixNum (Int5Bit n)) = "NFixNum(" ++ show n ++ ")"
msgShow (MsgU16 (Uint16 n)) = "U16(" ++ show n ++ ")"

instance Show Msg where
   show = msgShow

instance Arbitrary Int7Bit where
  arbitrary = do
		n <- choose (0,127) :: Gen Int
		return $ Int7Bit n

instance Arbitrary Int5Bit where
  arbitrary = do
		n <- choose (0,31) :: Gen Int
		return $ Int5Bit n

instance Arbitrary Uint16 where
  arbitrary = do
		n <- choose (0,65535) :: Gen Int
		return $ Uint16 n

instance Arbitrary Msg where
  arbitrary = do
    n <- choose (1,7) :: Gen Int
    case n of
      1 -> return MsgNil
      2 -> return MsgFalse
      3 -> return MsgTrue
      4 -> do
        l <- choose (1,5) :: Gen Int
        list <- sequence $ [arbitrary :: Gen Msg | _ <- [1..l]]
        return $ MsgFixArray list
      5 -> do
        n <- arbitrary
        return $ MsgPFixNum n
      6 -> do
        n <- arbitrary
        return $ MsgNFixNum n
      7 -> do
        n <- arbitrary
        return $ MsgU16 n

main = do
  msges <- sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..1000]] :: IO [Msg]
  forM_ msges (\msg -> print $ msg)
