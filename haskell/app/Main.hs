module Main where

import Control.Monad (void, when)
import Data.Char (chr, ord)
import Data.Word (Word8)
import System.Environment (getArgs, getProgName)

tokenChars :: String
tokenChars = "><+-.,[]"

data Operator
  = Next
  | Prev
  | Increment
  | Decrement
  | Output
  | Input
  | Loop [Operator]
  deriving (Eq, Show)

type Program = [Operator]

data Tape = Tape [Word8] Word8 [Word8] deriving (Show, Eq)

moveR :: Tape -> Tape
moveR (Tape l x (r : rs)) = Tape (x : l) r rs
moveR (Tape l x []) = Tape (x : l) 0 []

moveL :: Tape -> Tape
moveL (Tape (l : ls) x rs) = Tape ls l (x : rs)
moveL (Tape [] x r) = Tape [] 0 (x : r)

modify :: (Word8 -> Word8) -> Tape -> Tape
modify f (Tape l x r) = Tape l (f x) r

initialTape :: Tape
initialTape = Tape [] 0 []

current :: Tape -> Word8
current (Tape _ x _) = x

data Error
  = InvalidInputValue
  | UnknownToken Char
  deriving (Eq, Show)

type Result a = Either Error a

data Segment = Literal String | Nested Segment deriving (Show, Eq)

tokenToOp :: Char -> Operator
tokenToOp c
  | c == '>' = Next
  | c == '<' = Prev
  | c == '+' = Increment
  | c == '-' = Decrement
  | c == '.' = Output
  | c == ',' = Input
  | otherwise = error $ "Invalid tokken: " <> show c

parse :: String -> Program
parse src = case parseBlock (filter (`elem` tokenChars) src) of
  (prog, "") -> prog
  (_, rest) -> error $ "Unexpected token: " <> rest
 where
  parseBlock :: String -> (Program, String)
  parseBlock "" = ([], "")
  parseBlock (c : cs)
    | c == ']' = ([], cs)
    | c == '[' =
        let (inner, rest1) = parseBlock cs
            (outer, rest2) = parseBlock rest1
         in (Loop inner : outer, rest2)
    | otherwise =
        let op = tokenToOp c
            (ops, rest) = parseBlock cs
         in (op : ops, rest)

evaluate :: Operator -> Tape -> IO Tape
evaluate op t = case op of
  Next -> pure (moveR t)
  Prev -> pure (moveL t)
  Increment -> pure (modify (+ 1) t)
  Decrement -> pure (modify (subtract 1) t)
  Output -> putChar (chr $ fromIntegral (current t)) >> pure t
  Input -> do
    c <- getChar
    pure (modify (const (fromIntegral $ ord c)) t)
  Loop body -> loop t
   where
    loop :: Tape -> IO Tape
    loop tape
      | current tape == 0 = pure tape
      | otherwise = do
          tape' <- run body tape
          loop tape'

run :: Program -> Tape -> IO Tape
run [] tape = pure tape
run (op : ops) tape = evaluate op tape >>= run ops

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  when (length args /= 1) (error $ "Usage: " <> prog <> " <.bf file path>")
  src <- readFile (head args)
  let program = parse src
  print program
  finalState <- run program initialTape
  print finalState
