{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bits (testBit)
import System.Environment (getArgs)

import Text.Parsec (ParseError, parse, many1)
import qualified Text.Parsec.Token as Token
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Language (haskellDef)

data Test = Test { n  :: Int
                 , p1 :: Int
                 , p2 :: Int } deriving Show

lexer = Token.makeTokenParser haskellDef
natural = Token.natural lexer
comma = Token.comma lexer

testDsc :: Parser Test
testDsc = intParse >>= \n -> comma >>
          intParse >>= \p1 -> comma >>
          intParse >>= \p2 -> return $ Test n p1 p2
  where
    intParse = fmap fromInteger natural

printAnswer :: Test -> IO ()
printAnswer Test { .. } = putStrLn answer
  where
    testBitInteger = testBit (n + 1)
    answer = if testBitInteger p1 == testBitInteger p2 then "true" else "false"

job :: String -> IO ()
job fileName = parseFromFile (many1 testDsc) fileName >>= \res -> case res of
    Left err -> do
        error $ show err
    Right res -> mapM_ printAnswer res

main :: IO ()
main = getArgs >>= \args -> case args of
    [] -> error "no file name for parse"
    fileName:_ -> job fileName
