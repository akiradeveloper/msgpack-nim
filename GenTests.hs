import Data.List
import Test.QuickCheck
import Control.Monad
import Data.Bits
import Data.Word

data Msg =
    MsgFixArray [Msg]
  | MsgArray16 [Msg]
  | MsgArray32 [Msg]
  | MsgFixMap [(Msg, Msg)]
  | MsgMap16 [(Msg, Msg)]
  | MsgMap32 [(Msg, Msg)]
  | MsgNil
  | MsgTrue
  | MsgFalse
  | MsgPFixNum Int
  | MsgNFixNum Int
  | MsgUInt8 Int
  | MsgUInt16 Int
  | MsgUInt32 Int
  | MsgUInt64 Word
  | MsgInt8 Int
  | MsgInt16 Int
  | MsgInt32 Int
  | MsgInt64 Int
  | MsgFloat32 Float
  | MsgFloat64 Double
  | MsgFixStr String
  | MsgStr8 String
  | MsgStr16 String
  | MsgStr32 String
  | MsgBin8 [Int]
  | MsgBin16 [Int]
  | MsgBin32 [Int]
  -- fixext1
  -- fixext2
  -- fixext4
  -- fixext8
  -- fixext16
  -- ext8
  -- ext16
  -- ext32

arrayShow :: [Msg] -> String
arrayShow xs = intercalate "," $ map msgShow xs

mapShow :: [(Msg, Msg)] -> String
mapShow xs = intercalate "," $ map (\(k, v) -> "(" ++ msgShow k ++ "," ++  msgShow v ++ ")") xs

msgShow :: Msg -> String
msgShow (MsgFixArray xs) = "FixArray(toIterable(@[" ++ arrayShow xs ++ "]))"
msgShow (MsgArray16 xs) = "Array16(toIterable(@[" ++ arrayShow xs ++ "]))"
msgShow (MsgArray32 xs) = "Array32(toIterable(@[" ++ arrayShow xs ++ "]))"
msgShow (MsgFixMap xs) = "FixMap(toIterable(@[" ++ mapShow xs ++ "]))"
msgShow (MsgMap16 xs) = "Map16(toIterable(@[" ++ mapShow xs ++ "]))"
msgShow (MsgMap32 xs) = "Map32(toIterable(@[" ++ mapShow xs ++ "]))"
msgShow MsgNil = "Nil"
msgShow MsgTrue = "True"
msgShow MsgFalse = "False"
msgShow (MsgPFixNum n) = "PFixNum(" ++ show n ++ "'u8)"
msgShow (MsgNFixNum n) = "NFixNum(" ++ show n ++ "'i8)"
msgShow (MsgUInt8 n) = "UInt8(" ++ show n ++ "'u8)"
msgShow (MsgUInt16 n) = "UInt16(" ++ show n ++ "'u16)"
msgShow (MsgUInt32 n) = "UInt32(" ++ show n ++ "'u32)"
msgShow (MsgUInt64 n) = "UInt64(" ++ show n ++ "'u64)"
msgShow (MsgInt8 n) = "Int8(" ++ show n ++ "'i8)"
msgShow (MsgInt16 n) = "Int16(" ++ show n ++ "'i16)"
msgShow (MsgInt32 n) = "Int32(" ++ show n ++ "'i32)"
msgShow (MsgInt64 n) = "Int64(" ++ show n ++ "'i64)"
msgShow (MsgFloat32 n) = "Float32(" ++ show n ++ ")"
msgShow (MsgFloat64 n) = "Float64(" ++ show n ++ ")"
msgShow (MsgFixStr s) = "FixStr(" ++ show s ++ ")"
msgShow (MsgStr8 s) = "Str8(" ++ show s ++ ")"
msgShow (MsgStr16 s) = "Str16(" ++ show s ++ ")"
msgShow (MsgStr32 s) = "Str32(" ++ show s ++ ")"
msgShow (MsgBin8 xs) = "Bin8(@[" ++ (intercalate "," $ map (\x -> "cast[byte](" ++ show x ++ ")") xs) ++ "])"
msgShow (MsgBin16 xs) = "Bin16(@[" ++ (intercalate "," $ map (\x -> "cast[byte](" ++ show x ++ ")") xs) ++ "])"
msgShow (MsgBin32 xs) = "Bin32(@[" ++ (intercalate "," $ map (\x -> "cast[byte](" ++ show x ++ ")") xs) ++ "])"

instance Show Msg where
  show = msgShow

randStr :: Int -> Gen String
randStr n = sequence [choose ('a', 'Z') | _ <- [1..n]]

randBinSeq :: Int -> Gen [Int]
randBinSeq n = sequence [choose (0, 255) | _ <- [1..n]]

randMsg :: Int -> Gen [Msg]
randMsg n = sequence [arbitrary | _ <- [1..n]]

randMap :: Int -> Gen [(Msg, Msg)]
randMap n = do
    xs <- randMsg n
    ys <- randMsg n
    return $ zip xs ys

-- array weight
aw = 1

-- map weight
mw = 1

-- value weight
vw = 3

-- max signed
maxS n = (1 `shiftL` (n - 1)) - 1

-- max unsigned
maxUS n = (1 `shiftL` n) - 1

instance Arbitrary Msg where 
  arbitrary = do
    frequency [
        (aw, liftM MsgFixArray $ choose (1, 7) >>= randMsg)
      , (aw, liftM MsgArray16 $ choose (1, 10) >>= randMsg)
      , (aw, liftM MsgArray32 $ choose (1, 10) >>= randMsg)
      , (mw, liftM MsgFixMap $ choose (1, 5) >>= randMap)
      , (mw, liftM MsgMap16 $ choose (1, 10) >>= randMap)
      , (mw, liftM MsgMap32 $ choose (1, 10) >>= randMap)
      , (vw, return MsgNil)
      , (vw, return MsgTrue)
      , (vw, return MsgFalse)
      , (vw, liftM MsgPFixNum $ choose (0, 63))
      , (vw, liftM MsgNFixNum $ choose (-32, 0))
      , (vw, liftM MsgUInt8 $ choose (0, maxUS 8))
      , (vw, liftM MsgUInt16 $ choose (0, maxUS 16))
      , (vw, liftM MsgUInt32 $ choose (0, maxUS 32))
      , (vw, liftM MsgUInt64 $ choose (0, maxUS 63))
      , (vw, liftM MsgInt8 $ choose (-127, 127))
      , (vw, liftM MsgInt16 $ choose (-127, 127))
      , (vw, liftM MsgInt32 $ choose (-127, 127))
      , (vw, liftM MsgInt64 $ choose (-127, 127))
      , (vw, liftM MsgFloat32 $ arbitrary)
      , (vw, liftM MsgFloat64 $ arbitrary)
      , (vw, liftM MsgFixStr $ choose (0, 31) >>= randStr)
      , (vw, liftM MsgStr8 $ choose (0, 31) >>= randStr)
      , (vw, liftM MsgStr16 $ choose (0, 31) >>= randStr)
      , (vw, liftM MsgStr32 $ choose (0, 31) >>= randStr)
      , (vw, liftM MsgBin8 $ choose (0, 10) >>= randBinSeq)
      , (vw, liftM MsgBin16 $ choose (0, 10) >>= randBinSeq)
      , (vw, liftM MsgBin32 $ choose (0, 10) >>= randBinSeq)
      ]

main = do
  msges <- sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..1000]] :: IO [Msg]
  forM_ msges (\msg -> print $ msg)
