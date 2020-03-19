module Operators where

normalOpApp :: Int -> Int -> Int
normalOpApp x y = x + y

prefixOpApp :: Int -> Int -> Int
prefixOpApp x y = (+) x y

etaOpApp :: Int -> Int -> Int
etaOpApp = (+)

functionAsOp :: Int -> Int -> Int
functionAsOp x y = x `mod` y

badParensOp :: Int -> Int -> Int
badParensOp x y = ( +) x y

badBackticksFun :: Int -> Int -> Int
badBackticksFun x y = x `  mod ` y
