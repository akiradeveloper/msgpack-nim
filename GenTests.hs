import Data.List
import Test.QuickCheck
import Control.Monad

data Msg =
    MsgNil
  | MsgFalse
  | MsgTrue
  | MsgFixArray [Msg]

msgShow MsgNil = "Nil()"
msgShow MsgFalse = "False()"
msgShow MsgTrue = "True()"
msgShow (MsgFixArray xs) = "FixArray(@[" ++ (intercalate "," $ map msgShow xs) ++ "])"

instance Show Msg where
   show = msgShow

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
  msges <- sequence $ [generate (arbitrary :: Gen Msg) | _ <- [1..1000]] :: IO [Msg]
  forM_ msges (\msg -> print $ msg)
