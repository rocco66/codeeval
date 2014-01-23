module BitPosition where

import Data.Bits (testBit)
import Data.Text.Lazy (Text)
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)
import System.Exit (exitWith)

import Text.Parsec (ParseError, parse, many1, newline)
import Text.Parsec.Token (natural, comma)

data Test = Test { n  :: Integer
                 , p1 :: Int
                 , p2 :: Int }

testDsc = do
    n <- natural
    comma
    p1 <- natural
    comma
    p2 <- natural
    newline
    return Test { .. }

parseFile :: Text -> Either ParseError [Test]
parseFile = parse (many1 testDsc) "wrong test file"

printAnswer :: Test -> IO ()
printAnswer Test { .. } = putStrLn answer
  where
    answer = if testBit n p1 == testBit n p2 then "true" else "false"

main :: IO ()
main = getArgs >>= \case
    [] -> exitWith 1
    fileName:_ -> do
        fileContent <- TextIO.readFile fileName
        case parseFile fileContent of
          Left _ -> exitWith 1
          Right res -> mapM_ printAnswer res
