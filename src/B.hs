--B.hs
module B where

import Data.Semigroup

semop :: Sum Int -> Sum Int -> Sum Int
semop x y = (<>) x y

main = print "Hello, World!"
