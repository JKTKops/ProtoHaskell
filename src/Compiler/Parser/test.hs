module Test where

--- test of comment

{- TEST: Block comment.
-}

{- TEST: Nested block comment.
{- this portion is nested.
-}
this is still commented.
-}

test :: a
test = undefined

f x = case x of
      True -> 0
      False -> 1
