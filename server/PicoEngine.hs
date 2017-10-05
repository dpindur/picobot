module PicoEngine
( Match (..)
, Action (..)
, Rule (..)
, GameState (..)
, Pico (..)
, Game (..)
, Board
, State
, runGame
) where

import qualified Prelude
import Prelude hiding (Left, Right)
import qualified Control.Monad.State as S
import Control.Lens as L
import Data.List (find)

-- Data declarations
data Match = Always | Empty | Blocked deriving (Show)
data Action = Left | Right | Up | Down | None deriving (Show)
data GameState = Play | Success | Error String deriving (Show)

data Rule = Rule { up     :: Match
                 , right  :: Match
                 , down   :: Match
                 , left   :: Match
                 , action :: Action
                 , next   :: Int
                 } deriving (Show)

data Pico = Pico { x         :: Int
                 , y         :: Int
                 , picoState :: Int
                 } deriving (Show)

data Game = Game { pico        :: Pico
                 , board       :: Board
                 , gameState   :: GameState
                 , stepCounter :: Int
                 } deriving (Show)

-- Type aliases                 
type State = [Rule]
type Board = [[Int]]

-- Function to step pico forward, looks for a matching rule in the state provided
-- Errors if no rule matches the current position and state
stepPico :: Pico -> State -> Board -> (Pico, GameState)
stepPico p s b = case find (ruleMatch (x p) (y p) b) s of
        Nothing   -> (p, Error "No rule matches the current position")
        Just rule -> movePico p (action rule) (next rule) b

-- Updates pico with the specified action
-- Errors if pico is attempting an illegal move
movePico :: Pico -> Action -> Int -> Board -> (Pico, GameState)
movePico p Left  n b = if isBlocked $ b ^? L.element (x p-1) . L.element (y p) 
                            then (p, Error "Cannot move left") 
                            else ((Pico { x = (x p-1), y = (y p), picoState = n }), Play)

movePico p Right n b = if isBlocked $ b ^? L.element (x p+1) . L.element (y p) 
                            then (p, Error "Cannot move right") 
                            else ((Pico { x = (x p+1), y = (y p), picoState = n }), Play)

movePico p Up    n b = if isBlocked $ b ^? L.element (x p) . L.element (y p-1) 
                            then (p, Error "Cannot move up")
                            else ((Pico { x = (x p), y = (y p-1), picoState = n }), Play)

movePico p Down  n b = if isBlocked $ b ^? L.element (x p) . L.element (y p+1) 
                            then (p, Error "Cannot move down")
                            else ((Pico { x = (x p), y = (y p+1), picoState = n }), Play)

movePico p None  n _ = ((Pico { x = (x p), y = (y p), picoState = n }), Play)        

-- Determines if a the current (x, y) position on the board matches the supplied rule
ruleMatch :: Int -> Int -> Board -> Rule -> Bool
ruleMatch x y b (Rule up right down left _ _) = 
    let upMatch = case up of
            Always  -> True
            Empty   -> b ^? L.element x . L.element (y-1) /= Just 1
            Blocked -> b ^? L.element x . L.element (y-1) == Just 1

        downMatch = case down of
            Always  -> True
            Empty   -> b ^? L.element x . L.element (y+1) /= Just 1
            Blocked -> b ^? L.element x . L.element (y+1) == Just 1

        leftMatch = case left of
            Always  -> True
            Empty   -> b ^? L.element (x-1) . L.element y /= Just 1
            Blocked -> b ^? L.element (x-1) . L.element y == Just 1

        rightMatch = case right of
            Always  -> True
            Empty   -> b ^? L.element (x+1) . L.element y /= Just 1
            Blocked -> b ^? L.element (x+1) . L.element y == Just 1
    in (upMatch && downMatch && rightMatch && leftMatch)

-- Used to determine if a cell is blocked
isBlocked :: Maybe Int -> Bool
isBlocked (Just 1) = True
isBlocked _        = False

-- Step the board forward, set the square pico is on to 2 (grey)
stepBoard :: Board -> Pico -> Board
stepBoard b p = case b ^? L.element (x p) . L.element (y p) of
                   Just 0 -> b & L.element (x p) . L.element (y p) .~ 2
                   _      -> b

-- Checks if the board is finished, board is finished when there are no white (0) cells left
checkBoard :: Board -> Bool
checkBoard = all (\xs -> all (/= 0) xs)

stepGame :: [(Int, State)] -> S.State Game GameState
stepGame states = do 
    game <- S.get
    let curState = lookup (picoState $ pico game) states
        pico'    = case curState of
                     Just s  -> stepPico (pico game) s (board game)
                     Nothing -> ((pico game), Error ("No rule for state " ++ (show $ picoState $ pico game)))
        newBoard = stepBoard (board game) (pico game)
        newState = if stepCounter game == 0 then Error "Out of time"
                   else if checkBoard newBoard then Success else snd pico'
        newPico  = fst pico'
    S.put Game {pico = newPico, board = newBoard, gameState = newState, stepCounter = (stepCounter game -1)}
    case newState of
        Play    -> stepGame states
        Success -> return newState
        Error _ -> return newState

states :: [(Int, State)]
states = [(0, state0), (1, state1), (2, state2)]

bar :: GameState
bar = S.evalState (stepGame states) defaultGame

runGame :: Board -> Pico -> [(Int, State)] -> Int -> GameState
runGame b p s maxSteps = S.evalState (stepGame s) Game { pico = p, board = b, gameState = Play, stepCounter = maxSteps }

state0 :: State
state0 = [ Rule 
           { up      = Always
           , right   = Empty
           , down    = Always
           , left    = Always
           , action  = Right
           , next    = 0
           }
         , Rule 
           { up      = Always
           , right   = Blocked
           , down    = Always
           , left    = Always
           , action  = Down
           , next    = 1
           }
         ]

state1 :: State
state1 = [ Rule
           { up      = Always
           , right   = Always
           , down    = Blocked
           , left    = Blocked
           , action  = Up
           , next    = 2
           }
         , Rule
           { up      = Always
           , right   = Always
           , down    = Always
           , left    = Empty
           , action  = Left
           , next    = 1
           }
         , Rule
           { up      = Always
           , right   = Always
           , down    = Always
           , left    = Blocked
           , action  = Down
           , next    = 0
           }
         ]

state2 :: State
state2 = [ Rule
           { up      = Empty
           , right   = Always
           , down    = Always
           , left    = Always
           , action  = Up
           , next    = 2
           }
         , Rule
           { up      = Blocked
           , right   = Empty
           , down    = Always
           , left    = Always
           , action  = None
           , next    = 0
           }
         ]   

main ::  IO ()
main = putStrLn (show bar)





defaultPico :: Pico
defaultPico = Pico { x = 1, y = 1, picoState = 0 }

defaultBoard :: Board
defaultBoard = [
              [1, 1, 1, 1, 1, 1, 1, 1]
             ,[1, 0, 0, 0, 0, 0, 0, 1]
             ,[1, 0, 0, 0, 0, 0, 0, 1]
             ,[1, 0, 0, 0, 0, 0, 0, 1]
             ,[1, 0, 0, 0, 0, 0, 0, 1]
             ,[1, 0, 0, 0, 0, 0, 0, 1]
             ,[1, 0, 0, 0, 0, 0, 0, 1]
             ,[1, 1, 1, 1, 1, 1, 1, 1]
             ]
             
defaultGame :: Game
defaultGame = Game { pico = defaultPico, board = defaultBoard, gameState = Play, stepCounter = 1000 }