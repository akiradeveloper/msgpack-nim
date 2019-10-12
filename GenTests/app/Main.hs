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
  | MsgFixExt1 Int [Int]
  | MsgFixExt2 Int [Int]
  | MsgFixExt4 Int [Int]
  | MsgFixExt8 Int [Int]
  | MsgFixExt16 Int [Int]
  | MsgExt8 Int [Int]
  | MsgExt16 Int [Int]
  | MsgExt32 Int [Int]

binShow :: [Int] -> String
binShow xs = intercalate "," $ map (\x -> "cast[byte](" ++ show x ++ ")") xs

arrayShow :: [Msg] -> String
arrayShow xs = intercalate "," $ map msgShow xs

mapShow :: [(Msg, Msg)] -> String
mapShow xs = intercalate "," $ map (\(k, v) -> "(" ++ msgShow k ++ "," ++  msgShow v ++ ")") xs

msgShow :: Msg -> String
msgShow (MsgFixArray xs) = "FixArray(@[" ++ arrayShow xs ++ "])"
msgShow (MsgArray16 xs) = "Array16(@[" ++ arrayShow xs ++ "])"
msgShow (MsgArray32 xs) = "Array32(@[" ++ arrayShow xs ++ "])"
msgShow (MsgFixMap xs) = "FixMap(@[" ++ mapShow xs ++ "])"
msgShow (MsgMap16 xs) = "Map16(@[" ++ mapShow xs ++ "])"
msgShow (MsgMap32 xs) = "Map32(@[" ++ mapShow xs ++ "])"
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
msgShow (MsgBin8 xs) = "Bin8(@[" ++ binShow xs ++ "])"
msgShow (MsgBin16 xs) = "Bin16(@[" ++ binShow xs ++ "])"
msgShow (MsgBin32 xs) = "Bin32(@[" ++ binShow xs ++ "])"
msgShow (MsgFixExt1 t xs) = "FixExt1((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"
msgShow (MsgFixExt2 t xs) = "FixExt2((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"
msgShow (MsgFixExt4 t xs) = "FixExt4((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"
msgShow (MsgFixExt8 t xs) = "FixExt8((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"
msgShow (MsgFixExt16 t xs) = "FixExt16((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"
msgShow (MsgExt8 t xs) = "Ext8((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"
msgShow (MsgExt16 t xs) = "Ext16((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"
msgShow (MsgExt32 t xs) = "Ext32((" ++ show t ++ "'i8" ++ ", @[" ++ binShow xs ++ "]))"

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
vw = 2

-- max signed
maxS n = (1 `shiftL` (n - 1)) - 1

-- max unsigned
maxUS n = (1 `shiftL` n) - 1

-- type is a signed 8-bit signed integer
-- type < 0 is reserved for future extension including 2-byte type information
extType :: Gen Int
extType = choose (0, 127)

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
      , (vw, liftM MsgNFixNum $ choose (-32, -1))
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
      , (vw, liftM2 MsgFixExt1 extType $ randBinSeq 1)
      , (vw, liftM2 MsgFixExt2 extType $ randBinSeq 2)
      , (vw, liftM2 MsgFixExt4 extType $ randBinSeq 4)
      , (vw, liftM2 MsgFixExt8 extType $ randBinSeq 8)
      , (vw, liftM2 MsgFixExt16 extType $ randBinSeq 16)
      , (vw, liftM2 MsgExt8 extType $ choose (0, 10) >>= randBinSeq)
      , (vw, liftM2 MsgExt16 extType $ choose (0, 10) >>= randBinSeq)
      , (vw, liftM2 MsgExt32 extType $ choose (0, 10) >>= randBinSeq)
      ]

main = do
  msges <- sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..1000]] :: IO [Msg]
  forM_ msges (\msg -> print $ msg)
