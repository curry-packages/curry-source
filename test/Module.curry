--- Simple example module with different kinds of declarations and comments.
--- 

module Module where

-- Single-line comment for a type declaration.
data MyData = MyData Int

{-
  Multi-line comment.

  Hello, world!
-}
newtype MyData2 = MyData2 Int

type MyFunctionalType = MyData -> MyData2

{-
  Multi-line comment.
-}
myOp1 :: Int -> Int
myOp1 = (+ 42)

--- Some comment for a function declaration 
--- with a newline. 
myOp2 :: Int -> Int
myOp2 = (+ 73)

myOp3 :: Int -> Int
myOp3 = const 42

--- Some infix operator:
(>?) :: Int -> Int -> Bool
(>?) = (>)

--- Some type class.
class ToInt a where
  coerce :: a -> Int

instance ToInt Int where
  coerce = id

instance ToInt MyData where
  coerce (MyData x) = x