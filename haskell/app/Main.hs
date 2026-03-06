{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (when)
import Data.Char (chr, ord)
import System.Environment (getArgs, getProgName)
import Text.Printf (IsChar (fromChar, toChar))

tokenChars :: String
tokenChars = "><+-.,[]"

data Operator
  = IncP
  | DecP
  | IncD
  | DecD
  | Out
  | In
  | Open
  | Close
  deriving (Eq, Show)

instance IsChar Operator where
  fromChar '>' = IncP
  fromChar '<' = DecP
  fromChar '+' = IncD
  fromChar '-' = DecD
  fromChar '.' = Out
  fromChar ',' = In
  fromChar '[' = Open
  fromChar ']' = Close
  fromChar _ = error "unknown token"

  toChar IncP = '>'
  toChar DecP = '<'
  toChar IncD = '+'
  toChar DecD = '-'
  toChar Out = '.'
  toChar In = ','
  toChar Open = '['
  toChar Close = ']'

type Memory = [Int]
type Pointer = Int
type Cursor = Int

data Tape = Tape [Int] Int [Int]

moveR :: Tape -> Tape
moveR (Tape l x (r : rs)) = Tape (x : l) r rs
moveR (Tape l x []) = Tape (x : l) 0 []

moveL :: Tape -> Tape
moveL (Tape (l : ls) x rs) = Tape ls l (x : rs)
moveL (Tape [] x r) = Tape [] 0 (x:r)

type Stack = [Int]

parse :: String -> [Operator]
parse = map fromChar . filter (`elem` tokenChars)

splice :: Int -> [Int] -> Int -> [Int]
splice i xs x = take i xs <> [x] <> drop (i + 1) xs

updateCell :: Memory -> Pointer -> (Int -> Int) -> Memory
updateCell m p f =
  let x = f (m !! p)
      x' = if x < 0 then 256 + x else x `mod` 256
   in splice p m x'

popHead :: [Int] -> Maybe (Int, [Int])
popHead [] = Nothing
popHead (x : xs) = Just (x, xs)

allocate :: Memory -> Int -> Memory
allocate m s = if length m == s then m else m <> [0]

data State = State
  { pointer :: Pointer
  , cursor :: Cursor
  , memory :: Memory
  , stack :: Stack
  , outputVal :: Maybe Char
  }
  deriving (Show, Eq)

initialState :: State
initialState = State 0 0 [0] [] Nothing

data Error = InvalidInputValue deriving (Eq, Show)

evaluate :: State -> Operator -> Maybe Char -> State
evaluate s@(State{cursor, pointer, memory}) IncP _ = s{cursor = cursor + 1, pointer = pointer + 1, memory = allocate memory (pointer + 2), outputVal = Nothing}
evaluate (State 0 _ _ _ _) DecP _ = error "Memory underflow"
evaluate s@(State{cursor, pointer}) DecP _ = s{cursor = cursor + 1, pointer = pointer - 1, outputVal = Nothing}
evaluate s@(State{cursor, pointer, memory}) IncD _ = s{cursor = cursor + 1, memory = updateCell memory pointer (+ 1), outputVal = Nothing}
evaluate s@(State{cursor, pointer, memory}) DecD _ = s{cursor = cursor + 1, memory = updateCell memory pointer (+ (-1)), outputVal = Nothing}
evaluate s@(State{cursor, pointer, memory}) Out _ = s{cursor = cursor + 1, outputVal = Just (chr $ memory !! pointer)}
evaluate _ In Nothing = error "Value not provided"
evaluate s@(State{cursor, pointer, memory}) In (Just v) = s{cursor = cursor + 1, memory = updateCell memory pointer (const (ord v)), outputVal = Nothing}
evaluate s@(State{cursor, stack}) Open _ = s{cursor = cursor + 1, stack = cursor : stack, outputVal = Nothing}
evaluate (State _ _ _ [] _) Close _ = error "Loop inconsistent"
evaluate s@(State{cursor, pointer, memory, stack}) Close _ = case popHead stack of
  Nothing -> error "Something wrong"
  Just (open, newStack) -> let newCusor = if memory !! pointer == 0 then cursor + 1 else open in s{cursor = newCusor, stack = newStack, outputVal = Nothing}

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
