--B.hs
module B where

import Data.Semigroup

data Thing = Thing

semop :: Sum Int -> Sum Int -> Sum Int
semop x y = (<>) x y

main = print "Hello, World!"

class YSem a where
  ymappend :: a -> a -> a
  lawAssoc :: a -> a -> a -> ()

instance YSem Int where
  ymappend = (+)
  lawAssoc _ _ _ = ()
