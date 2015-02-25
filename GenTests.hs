import Data.List
import Test.QuickCheck
import Control.Monad
import Data.Bits
import Data.Word

data Msg =
    MsgNil
  | MsgFalse
  | MsgTrue
  | MsgFixArray [Msg]
  | MsgPFixNum Int
  | MsgNFixNum Int
  | MsgU16 Int
  | MsgU32 Int
  | MsgU64 Word
  | MsgFixStr String

msgShow MsgNil = "Nil()"
msgShow MsgFalse = "False()"
msgShow MsgTrue = "True()"
msgShow (MsgFixArray xs) = "FixArray(@[" ++ (intercalate "," $ map msgShow xs) ++ "])"
msgShow (MsgPFixNum n) = "PFixNum(" ++ show n ++ "'u8)"
msgShow (MsgNFixNum n) = "NFixNum(" ++ show n ++ "'u8)"
msgShow (MsgU16 n) = "U16(" ++ show n ++ "'u16)"
msgShow (MsgU32 n) = "U32(" ++ show n ++ "'u32)"
msgShow (MsgU64 n) = "U64(" ++ show n ++ "'u64)"
msgShow (MsgFixStr s) = "FixStr(" ++ show s ++ ")"

instance Show Msg where
  show = msgShow

-- FIXME
randStr :: Int -> String
randStr n = map (\x -> 'a') [1..n]

instance Arbitrary Msg where 
  arbitrary = do
    n <- choose (1, 10) :: Gen Int
    case n of
      1 -> return MsgNil
      2 -> return MsgFalse
      3 -> return MsgTrue
      4 -> do
        l <- choose (1, 5) :: Gen Int
        list <- sequence $ [arbitrary :: Gen Msg | _ <- [1..l]]
        return $ MsgFixArray list
      5 -> do
        n <- choose (0, (1 `shiftL` 7)-1) :: Gen Int
        return $ MsgPFixNum n
      6 -> do
        n <- choose (0, (1 `shiftL` 5)-1) :: Gen Int
        return $ MsgNFixNum n
      7 -> do
        n <- choose (0, (1 `shiftL` 16)-1) :: Gen Int
        return $ MsgU16 n
      8 -> do
        n <- choose (0, (1 `shiftL` 32)-1) :: Gen Int
        return $ MsgU32 n
      9 -> do
        n <- choose (0, (1 `shiftL` 63)-1) :: Gen Word
        return $ MsgU64 n
      10 -> do
        n <- choose (0, 31) :: Gen Int
        return $ MsgFixStr $ randStr n
  
main = do
  msges <- sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..3000]] :: IO [Msg]
  forM_ msges (\msg -> print $ msg)
