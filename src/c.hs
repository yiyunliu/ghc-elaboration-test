--B.hs
module QQQ where

-- import qualified Data.Semigroup as DS
import Data.Semigroup
data Thing = Thing

-- semop :: DS.Sum Int -> DS.Sum Int -> DS.Sum Int
-- semop x y = (<>) x y

main = print "Hello, World!"

class YSem a where
  ymappend :: a -> a -> a
  lawAssoc :: a -> a -> a -> ()

instance YSem Int where
  ymappend = (+)
  lawAssoc _ _ _ = ()


testPlus :: Int -> Int
testPlus x = x + 1
