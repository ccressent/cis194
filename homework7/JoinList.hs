module JoinList where

import Data.Monoid

import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

sizeOf :: (Sized a) => a -> Int
sizeOf = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ n jl@(Append _ left right)
    | n >= sizeOf (tag jl)   = Nothing
    | n <  sizeOf (tag left) = indexJ n left
    | otherwise              = indexJ (n - sizeOf (tag left)) right

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ 0 jl           = jl
dropJ _ (Single _ _) = Empty
dropJ n jl@(Append _ left right)
    | n >= sizeOf (tag jl)   = Empty
    | n >  sizeOf (tag left) = Empty +++ dropJ (n - sizeOf (tag left)) right
    | otherwise              = dropJ n left +++ right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ 0 jl              = jl
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append _ left right)
    | n >= sizeOf (tag jl)   = jl
    | n >  sizeOf (tag left) = left +++ takeJ (n - sizeOf (tag left)) right
    | otherwise              = takeJ n left


scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine s  = Single (scoreString s) s


--
-- Everything below used to run manual tests in the REPL
--

(!!?) :: [a] -> Int -> Maybe a
[] !!? _         = Nothing
_  !!? i | i < 0 = Nothing
(x:_) !!? 0      = Just x
(_:xs) !!? i     = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty              = []
jlToList (Single _ a)       = [a]
jlToList (Append _ jl1 jl2) = jlToList jl1 ++ jlToList jl2

testJL :: JoinList Size Int
testJL = Append (Size 4) (Append (Size 2) (Single (Size 1) 0) (Single (Size 1) 1)) (Append (Size 2) (Single (Size 1) 2) (Single (Size 1) 3))

testJL2 :: JoinList Size Char
testJL2 = Append (Size 4)
            (Append (Size 3)
              (Single (Size 1) 'y')
              (Append (Size 2)
                (Single (Size 1) 'e')
                (Single (Size 1) 'a')))
            (Single (Size 1) 'h')
