{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (when)
import Data.Char (chr, ord)
import System.Environment (getArgs, getProgName)
import Text.Printf (IsChar (fromChar, toChar))

tokenChars :: String
tokenChars = "><+-.,[]"

data Operator
  = MemMoveRight
  | MemMoveLeft
  | Inc
  | Dec
  | Out
  | In
  | Open
  | Close
  deriving (Eq, Show)

instance IsChar Operator where
  fromChar '>' = MemMoveRight
  fromChar '<' = MemMoveLeft
  fromChar '+' = Inc
  fromChar '-' = Dec
  fromChar '.' = Out
  fromChar ',' = In
  fromChar '[' = Open
  fromChar ']' = Close
  fromChar _ = error "unknown token"

  toChar MemMoveRight = '>'
  toChar MemMoveLeft = '<'
  toChar Inc = '+'
  toChar Dec = '-'
  toChar Out = '.'
  toChar In = ','
  toChar Open = '['
  toChar Close = ']'

type Pointer = Int
type Cursor = Int

data Tape = Tape [Int] Int [Int] deriving (Show, Eq)

moveR :: Tape -> Tape
moveR (Tape l x (r : rs)) = Tape (x : l) r rs
moveR (Tape l x []) = Tape (x : l) 0 []

moveL :: Tape -> Tape
moveL (Tape (l : ls) x rs) = Tape ls l (x : rs)
moveL (Tape [] x r) = Tape [] 0 (x : r)

modify :: (Int -> Int) -> Tape -> Tape
modify f (Tape l x r) = Tape l (f x `mod` 256) r

initialTape :: Tape
initialTape = Tape [] 0 []

current :: Tape -> Int
current (Tape _ x _) = x

type Stack = [Int]

parse :: String -> [Operator]
parse = map fromChar . filter (`elem` tokenChars)

splice :: Int -> [Int] -> Int -> [Int]
splice i xs x = take i xs <> [x] <> drop (i + 1) xs

popHead :: [Int] -> Maybe (Int, [Int])
popHead [] = Nothing
popHead (x : xs) = Just (x, xs)

data State = State
  { cursor :: Cursor
  , memory :: Tape
  , stack :: Stack
  , outputVal :: Maybe Char
  }
  deriving (Show, Eq)

initialState :: State
initialState = State 0 initialTape [] Nothing

data Error = InvalidInputValue deriving (Eq, Show)

evaluate :: State -> Operator -> Maybe Char -> State
evaluate s@(State{cursor, memory}) MemMoveRight _ = s{cursor = cursor + 1, memory = moveR memory, outputVal = Nothing}
evaluate s@(State{cursor, memory}) MemMoveLeft _ = s{cursor = cursor + 1, memory = moveL memory, outputVal = Nothing}
evaluate s@(State{cursor, memory}) Inc _ = s{cursor = cursor + 1, memory = modify (+ 1) memory, outputVal = Nothing}
evaluate s@(State{cursor, memory}) Dec _ = s{cursor = cursor + 1, memory = modify (+ (-1)) memory, outputVal = Nothing}
evaluate s@(State{cursor, memory}) Out _ = s{cursor = cursor + 1, outputVal = Just (chr $ current memory)}
evaluate _ In Nothing = error "Value not provided"
evaluate s@(State{cursor, memory}) In (Just v) = s{cursor = cursor + 1, memory = modify (const (ord v)) memory, outputVal = Nothing}
evaluate s@(State{cursor, stack}) Open _ = s{cursor = cursor + 1, stack = cursor : stack, outputVal = Nothing}
evaluate (State _ _ [] _) Close _ = error "Loop inconsistent"
evaluate s@(State{cursor, memory, stack}) Close _ = case popHead stack of
  Nothing -> error "Something wrong"
  Just (open, newStack) -> let newCusor = if current memory == 0 then cursor + 1 else open in s{cursor = newCusor, stack = newStack, outputVal = Nothing}

printOutputValue :: State -> IO ()
printOutputValue s = case outputVal s of
  Just v -> putStr $ show v
  Nothing -> return ()

run :: [Operator] -> Int -> State -> IO State
run ops currentCur s = do
  let nextCursor = cursor s
  if nextCursor >= length ops
    then
      return s
    else case ops !! currentCur of
      Out -> printOutputValue s >> run ops nextCursor (evaluate s (ops !! nextCursor) Nothing)
      In -> do
        v <- getChar
        run ops nextCursor (evaluate s (ops !! nextCursor) (Just v))
      _ -> run ops nextCursor (evaluate s (ops !! nextCursor) Nothing)

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  when (length args /= 1) (error $ "Usage: " <> prog <> " <.bf file path>")
  src <- readFile (head args)
  print $ parse src
  print "==="
  state <- run (parse src) 0 initialState
  print ""
  print "==="
  print state
