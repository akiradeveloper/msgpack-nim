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
  | MsgU8 Int
  | MsgU16 Int
  | MsgU32 Int
  | MsgU64 Word
  | MsgFixStr String
  | MsgFixMap [(Msg, Msg)]
  | MsgFloat32 Float
  | MsgFloat64 Double
  | MsgBin8 [Int]

arrayShow :: [Msg] -> String
arrayShow xs = intercalate "," $ map msgShow xs

mapShow :: [(Msg, Msg)] -> String
mapShow xs = intercalate "," $ map (\(k, v) -> "(" ++ msgShow k ++ "," ++  msgShow v ++ ")") xs

msgShow :: Msg -> String
msgShow MsgNil = "Nil()"
msgShow MsgFalse = "False()"
msgShow MsgTrue = "True()"
msgShow (MsgFixArray xs) = "FixArray(@[" ++ arrayShow xs ++ "])"
msgShow (MsgPFixNum n) = "PFixNum(" ++ show n ++ "'u8)"
msgShow (MsgNFixNum n) = "NFixNum(" ++ show n ++ "'u8)"
msgShow (MsgU8 n) = "U8(" ++ show n ++ "'u8)"
msgShow (MsgU16 n) = "U16(" ++ show n ++ "'u16)"
msgShow (MsgU32 n) = "U32(" ++ show n ++ "'u32)"
msgShow (MsgU64 n) = "U64(" ++ show n ++ "'u64)"
msgShow (MsgFixStr s) = "FixStr(" ++ show s ++ ")"
msgShow (MsgFixMap xs) = "Fixmap(@[" ++ mapShow xs ++ "])"
msgShow (MsgFloat32 n) = "Float32(" ++ show n ++ ")"
msgShow (MsgFloat64 n) = "Float64(" ++ show n ++ ")"
msgShow (MsgBin8 xs) = "Bin8(@[" ++ (intercalate "," $ map (\x -> "cast[b8](" ++ show x ++ ")") xs) ++ "])"

instance Show Msg where
  show = msgShow

randStr:: Int -> Gen String
randStr n = sequence [choose ('a', 'Z') | _ <- [1..n]]

randBinSeq :: Int -> Gen [Int]
randBinSeq n = sequence [choose (0, 255) | _ <- [1..n]]

instance Arbitrary Msg where 
  arbitrary = do
    oneof [
        return MsgNil
      , return MsgFalse
      , return MsgTrue
      , do l <- choose (1, 7) :: Gen Int
           liftM MsgFixArray $ sequence $ [arbitrary :: Gen Msg | _ <- [1..l]]
      , liftM MsgPFixNum $ choose (0, (1 `shiftL` 7)-1)
      , liftM MsgNFixNum $ choose (0, (1 `shiftL` 5)-1)
      , liftM MsgU8 $ choose (0, (1 `shiftL` 8)-1)
      , liftM MsgU16 $ choose (0, (1 `shiftL` 16)-1)
      , liftM MsgU32 $ choose (0, (1 `shiftL` 32)-1)
      , liftM MsgU64 $ choose (0, (1 `shiftL` 63)-1)
      , do n <- choose (0, 31) :: Gen Int
           liftM MsgFixStr $ randStr n
      , liftM MsgFloat32 $ arbitrary
      , liftM MsgFloat64 $ arbitrary
      , do n <- choose (0, 10) :: Gen Word8
           liftM MsgBin8 $ randBinSeq (fromIntegral n)
          ]
main = do
  msges <- sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..1000]] :: IO [Msg]
  forM_ msges (\msg -> print $ msg)
