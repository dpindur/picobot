import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (..)
import Keyboard
import Dict exposing (..)
import Html exposing (div, button, text)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (..)
import String exposing (lines, join, startsWith, isEmpty, trimRight)

-- !! Maybe Remove
cellSize = 10
(width, height) = (8, 8)
-- !!

-- Handle updating the game
-- Step moves forward one step if the game is paused and is ignored otherwise
-- Run continually moved forward and is ignored if the game is paused
type Update = NoUpdate | Step | TogglePause | Run Float | SyncRules String | Load

-- Used by a Rule to match a block.
-- Matches are either 'Always' true,
-- 'Empty' meaning the block is not a 1
-- or 'Blocked' meaning the block is 1
type Match = Always | Empty | Blocked

-- Action pico can take after matching a rule
type Action = Left | Right | Up | Down | None

-- State of the game
-- Play - Game is running and Step updates are ignored
-- Pause - Game is paused and Run updates are ignored
-- Success - Game is over, all squares a filled
-- Error - There was an error in the rules, e.g. inexhaustive pattern, illegal move, no rule for a state
type GameState = Play | Pause | Success | Error String

-- A possible Rule pattern for a given State
type alias Rule = { up     : Match
                  , right  : Match
                  , down   : Match
                  , left   : Match
                  , action : Action
                  , next   : Int }

-- Each State contains a number of Rules
-- Rules are matched in order
type alias State = List Rule

-- Pico has his current location and a state
type alias Pico = { x : Int, y : Int, state : Int }

-- The board is a 2D grid of integers
-- 0 = white/empty
-- 1 = black/blocked
-- 2 = grey/filled
-- 3 = blue/pico (Note pico is not stored on the board, his number is inserted in the render function)
type alias Board = List (List Int)

-- A Game consists of Pico, the board and the state of the game (Play, pause, Error, Succes)
type alias Game = { pico : Pico
                  , board : Board
                  , gameState : GameState
                  , states : Dict Int State
                  , rulesText : String }



-- ===========DEFAULT GAME DATA===========

--Default Game States
defaultPico : Pico
defaultPico = { x = 1, y = 1, state = 0 }

defaultBoard : Board
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

defaultGame : Game
defaultGame = { pico = defaultPico, board = defaultBoard, gameState = Pause, states = defaultStates, rulesText = "" }



-- ===========GAME LOGIC FUNCTIONS===========

-- Function to step Pico forward
-- Returns new Pico and a new GameState
-- Will move GameState into Error if an illegal move is attempted or there is no matching rule
stepPico : Pico -> State -> Board -> (Pico, GameState)
stepPico p s b =
    case first (ruleMatch p.x p.y b) s of
      Nothing   -> (p, Error "No rule matches current position")
      Just rule -> movePico p (rule.action) (rule.next) b

-- Helper function to update Pico based on a given action
-- Takes Pico, Action, NextState, Board and Returns the new Pico + any errors
movePico : Pico -> Action -> Int -> Board -> (Pico, GameState)
movePico p d n b = case d of
              Left  -> if (itemAt (p.x-1) (p.y) b |> isBlocked) then (p, Error "Cannot move Left") else  ({ p | x <- p.x - 1, state <- n }, Play)
              Right -> if (itemAt (p.x+1) (p.y) b |> isBlocked) then (p, Error "Cannot move Right") else ({ p | x <- p.x + 1, state <- n }, Play)
              Up    -> if (itemAt (p.x) (p.y-1) b |> isBlocked) then (p, Error "Cannot move Up") else    ({ p | y <- p.y - 1, state <- n }, Play)
              Down  -> if (itemAt (p.x) (p.y+1) b |> isBlocked) then (p, Error "Cannot move Down") else  ({ p | y <- p.y + 1, state <- n }, Play)
              None  -> ({ p | state <- n }, Play)

-- Helper function to find a matching rule for Pico
-- Determines if a given rule matches the current context and returns True or False
ruleMatch : Int -> Int -> Board -> Rule -> Bool
ruleMatch x y b r =
    let
      {up,right,down,left} = r

      upMatch = case up of
        Always  -> True
        Empty   -> (itemAt x (y-1) b) /= Just 1
        Blocked -> (itemAt x (y-1) b) == Just 1

      downMatch = case down of
        Always  -> True
        Empty   -> (itemAt x (y+1) b) /= Just 1
        Blocked -> (itemAt x (y+1) b) == Just 1

      leftMatch = case left of
        Always  -> True
        Empty   -> (itemAt (x-1) y b) /= Just 1
        Blocked -> (itemAt (x-1) y b) == Just 1

      rightMatch = case right of
        Always  -> True
        Empty   -> (itemAt (x+1) y b) /= Just 1
        Blocked -> (itemAt (x+1) y b) == Just 1

    in (upMatch && downMatch && leftMatch && rightMatch)

-- Helper function to determine if a cell is blocked
-- Checks if cell == 1 otherwise it's not blocked
isBlocked : Maybe Int -> Bool
isBlocked c = case c of
                Nothing -> False
                Just x  -> x == 1

-- Function to step the Board forward
-- If Pico is on a cell, mark that cell as grey
stepBoard : Board -> Pico -> Board
stepBoard b p = case itemAt p.x p.y b of
                  Just 0 -> editItemAt p.x p.y 2 b
                  _      -> b

-- Function to step the Game forward
stepGame : Update -> Game -> Game
stepGame update game =
      case update of
        TogglePause -> togglePause game
        Step        -> step game
        Run _       -> play game
        SyncRules s -> syncRules s game
        Load        -> load game
        _           -> game

-- Handle the TogglePause update
togglePause : Game -> Game
togglePause game = let { pico, board, gameState } = game
                   in  if | gameState == Pause -> { game | gameState <- Play  }
                          | gameState == Play  -> { game | gameState <- Pause }
                          | otherwise          -> game

-- Handle the Step update
step : Game -> Game
step game = let { pico,board,gameState,states,rulesText } = game

                state = Dict.get pico.state states

                pico' = case state of
                          Just s  -> stepPico pico s board
                          Nothing -> (pico, Error ("No rule for state " ++ (toString pico.state)))

                board' = stepBoard board pico

            in  if gameState == Pause then
                  { game |
                      pico      <- fst pico'
                  ,   board     <- board'
                  ,   gameState <- case snd pico' of
                                    Error x -> Error x
                                    _       -> Pause
                  }
                else game

-- Handle the Play update
play : Game -> Game
play game = let { pico,board,gameState,states,rulesText } = game

                state = Dict.get pico.state states

                pico' = case state of
                          Just s  -> stepPico pico s board
                          Nothing -> (pico, Error ("No rule for state " ++ (toString pico.state)))

                board' = stepBoard board pico

            in  if gameState == Play then
                  { game |
                      pico      <- fst pico'
                  ,   board     <- board'
                  ,   gameState <- case snd pico' of
                                    Error x -> Error x
                                    _       -> Play
                  }
                else game

-- Handle the SyncRules update
syncRules : String -> Game -> Game
syncRules s game = { game | rulesText <- s }

-- Handle the Load update
-- Parse the states
load : Game -> Game
load game = let a = String.lines game.rulesText
                  |> List.map String.trimRight
                  |> List.filter (\x -> not (String.isEmpty x) && not (String.startsWith "#" x))
                b = createStateTable a Dict.empty
            in case b of
                  Err e -> { game | gameState <- Error e }
                  Ok  s -> { game | states <- s, gameState <- Pause }

createStateTable : List String -> Dict Int State -> Result String (Dict Int State)
createStateTable rules dict = case rules of
                                []      -> Ok dict
                                (x::xs) -> case rule x of
                                              Err e     -> Err e
                                              Ok (r, _) -> case addRule dict (tokenToRule r) of
                                                            Err e -> Err e
                                                            Ok  d -> createStateTable xs d

addRule : Dict Int State -> (Int, Rule) -> Result String (Dict Int State)
addRule dict (state, rule) = let existingState = get state dict
                                 foo = case existingState of
                                          Nothing -> Dict.insert state [rule] dict
                                          Just s  -> Dict.insert state (rule::s) dict
                             in Ok foo




tokenToRule : List Token -> (Int, Rule)
tokenToRule ((PState s)::(Direction u)::(Direction r)::(Direction d)::(Direction l)::(PAction a)::(PState n)::_) =
                  (s,
                    { up = u
                    , right = r
                    , down = d
                    , left = l
                    , action = a
                    , next = n
                    }
                  )


--(toString (rule ("123 **** -> S 123")))
--rule : String -> Result String (List Token, String)

-- ===========SIGNAL FUNCTIONS===========

-- Game signal for running the game on at an fps
running : Signal Update
running = Signal.map Run (fps 10)

-- Game signal for stepping the game once when space is pressed
stepping : Signal Update
stepping = Signal.map (\x -> if x then Step else NoUpdate) Keyboard.space

-- Game signal for pausing and unpausing the game
pausing : Signal Update
pausing = Signal.map (\x -> if x then TogglePause else NoUpdate) (Keyboard.isDown 80)

-- Mailbox for getting inputs from the buttons
btnsMailbox : Signal.Mailbox Update
btnsMailbox = Signal.mailbox NoUpdate

-- Mailbox for getting the rules from the text area
rulesMailbox : Signal.Mailbox Update
rulesMailbox = Signal.mailbox NoUpdate

-- Helper function to extract the rules from the text area into a Signal
-- This is used to synchronise the textbox rules with the rules in the model
-- Rules are parsed and loaded via a different Signal, not this one
ruleSignal : String -> Signal.Message
ruleSignal x = Signal.message rulesMailbox.address (SyncRules x)

-- Update signal for game, merges stepping and running, preference is given to stepping
updating : Signal Update
updating = Signal.mergeMany [rulesMailbox.signal, btnsMailbox.signal, pausing, stepping, running]

-- State of the game, foldp up the step function, a default game and our updating signal
gameState : Signal Game
gameState = Signal.foldp stepGame defaultGame updating

-- Main method, display the gameState
main = Signal.map display gameState



-- ===========RENDERING FUNCTIONS===========

-- Function to render the Game
display : Game -> Html.Html
display {pico,board,gameState,states,rulesText} =
                              div []
                              [
                                Html.fromElement <| flow right
                                [
                                 (renderGrid pico board
                                 |> container (cellSize * width) (cellSize * height) topLeft)
                                , (show gameState)
                                ]
                              , button [ onClick btnsMailbox.address TogglePause ] [ Html.text "Go" ]
                              , button [ onClick btnsMailbox.address Step ] [ Html.text "Step" ]
                              , button [ onClick btnsMailbox.address Load ] [ Html.text "Load" ]
                              , Html.textarea [ placeholder "Enter Rules"
                                              , on "input" targetValue ruleSignal
                                              ] []
                              , Html.text (toString states)
                              ]

-- Draw Pico and the Board
renderGrid : Pico -> Board -> Element
renderGrid pico grid =
  grid
    |> editItemAt pico.x pico.y 3  -- Inject Pico
    |> List.map renderRow          -- Render each Row
    |> List.map (flow right)       -- Put each cell into a right flow
    |> flow down                   -- Put all these in a down flow

-- Helper function to render a row of Ints
renderRow : List Int -> List Element
renderRow row = List.map renderCell row

-- Helper function to render a single cell
renderCell : Int -> Element
renderCell cell =
  spacer cellSize cellSize
    |> color (if | cell == 0 -> (rgb 255 255 255)  -- White
                 | cell == 1 -> (rgb 0 0 0)        -- Black
                 | cell == 2 -> (rgb 128 128 128)  -- Grey
                 | cell == 3 -> (rgb 0 0 255))     -- Blue (Pico)



-- ===========HELPER FUNCTIONS FOR DEALING WITH LISTS===========

-- Helper function to find the first element of a list that satisfies a predicate
first : (a -> Bool) -> List a -> Maybe a
first p l = case l of
              []      -> Nothing
              (x::xs) -> if p x then Just x else first p xs

-- Helper function to access an item in a grid
-- Returns Nothing if the coordinates were outside the grid
-- Grid starts at x = 0, y = 0
itemAt : Int -> Int -> List (List a) -> Maybe a
itemAt x y grid = let row = case List.head (List.drop y grid) of
                              Just x  -> x
                              Nothing -> []
                  in  List.head (List.drop x row)

-- Helper function to edit an item in a grid
-- Returns the grid unchanged if the coordinates were outside the grid
-- Grid starts at x = 0, y = 0
editItemAt : Int -> Int -> a -> List (List a) -> List (List a)
editItemAt x y i grid = let row = case List.head (List.drop y grid) of
                                    Just x  -> x
                                    Nothing -> []
                            newList = (List.take x row) ++ [i] ++ (List.drop (x+1) row)
                         in (List.take y grid) ++ [newList] ++ (List.drop (y+1) grid)


-- ===============PARSING==============

type Token = PState Int | Direction Match | PAction Action

andThen f a = Result.andThen a f

toInt : String -> Maybe Int
toInt s = case String.toInt s of
             Err _ -> Nothing
             Ok  x -> Just x

return : String -> (List Token, String)
return a = ([], a)

state : Char -> (List Token, String) -> Result String (List Token, String)
state c a = let (token, xs) = a
            in  case xs of
                  ""     -> Err "Error on state"
                  x      -> case parseState (String.fromChar c) x of
                              (Nothing, _)   -> Err "Error on state, not a number"
                              (Just s, rest) -> Ok (token ++ [PState s], rest)

-- Helper function for parsing state
-- Takes a separator string, a string to parse and returns Maybe the state, plus the remaining string
parseState : String -> String -> (Maybe Int, String)
parseState c xs = let
                   split = String.split c xs
                   state = Maybe.andThen (List.head split) toInt
                   tail  = split |> List.tail
                   remaining = case tail of
                     Nothing -> ""
                     Just x  -> String.join c x
                  in (state, remaining)

char : Char -> (List Token, String) -> Result String (List Token, String)
char c a = let (token, xs) = a
           in  case xs of
                 "" -> Err "Error on char"
                 x  -> case String.uncons x of
                            Nothing    -> Err "Error on direction"
                            Just (h,s) -> if h == c then
                                            Ok (token, s)
                                          else
                                            Err ("Expected char '" ++ (String.fromChar c) ++ "'")

direction : (List Token, String) -> Result String (List Token, String)
direction a = let (token, xs) = a
              in  case xs of
                    "" -> Err "Error on direction"
                    x  -> case String.uncons x of
                            Nothing    -> Err "Error on direction"
                            Just (h,s) -> if | h == 'x'  -> Ok (token ++ [Direction Empty], s)
                                             | h == '*'  -> Ok (token ++ [Direction Always], s)
                                             | h == 'U'  -> Ok (token ++ [Direction Blocked], s)
                                             | h == 'D'  -> Ok (token ++ [Direction Blocked], s)
                                             | h == 'L'  -> Ok (token ++ [Direction Blocked], s)
                                             | h == 'R'  -> Ok (token ++ [Direction Blocked], s)
                                             | otherwise -> Err "Error on direction"

action : (List Token, String) -> Result String (List Token, String)
action a = let (token, xs) = a
              in  case xs of
                    "" -> Err "Error on action"
                    x  -> case String.uncons x of
                            Nothing    -> Err "Error on action"
                            Just (h,s) -> if | h == 'U'  -> Ok (token ++ [PAction Up], s)
                                             | h == 'D'  -> Ok (token ++ [PAction Down], s)
                                             | h == 'L'  -> Ok (token ++ [PAction Left], s)
                                             | h == 'R'  -> Ok (token ++ [PAction Right], s)
                                             | h == 'S'  -> Ok (token ++ [PAction None], s)
                                             | otherwise -> Err "Error on action"

rule : String -> Result String (List Token, String)
rule s = s |> return
           |> (state ' ')
           |> andThen direction
           |> andThen direction
           |> andThen direction
           |> andThen direction
           |> andThen (char ' ')
           |> andThen (char '-')
           |> andThen (char '>')
           |> andThen (char ' ')
           |> andThen action
           |> andThen (char ' ')
           |> andThen (state ' ')

-- ===========TESTING STATES===========

-- !! Testing States
state0 : State
state0 = [ { up      = Always
           , right   = Empty
           , down    = Always
           , left    = Always
           , action  = Right
           , next    = 0
           }
         , { up      = Always
           , right   = Blocked
           , down    = Always
           , left    = Always
           , action  = Down
           , next    = 1
           }
         ]

state1 : State
state1 = [ { up      = Always
           , right   = Always
           , down    = Blocked
           , left    = Blocked
           , action  = Up
           , next    = 2
           }
         , { up      = Always
           , right   = Always
           , down    = Always
           , left    = Empty
           , action  = Left
           , next    = 1
           }
         , { up      = Always
           , right   = Always
           , down    = Always
           , left    = Blocked
           , action  = Down
           , next    = 0
           }
         ]

state2 : State
state2 = [ { up      = Always
           , right   = Always
           , down    = Always
           , left    = Always
           , action  = Left
           , next    = 2
           }
         , { up      = Blocked
           , right   = Always
           , down    = Always
           , left    = Empty
           , action  = None
           , next    = 0
           }
         ]
-- !! End Testing States

-- Each state consists of an Integer and a List of Rules for that state
-- !! Might need to move this into the Game object...
defaultStates : Dict Int State
defaultStates = Dict.fromList [ (0, state0), (1, state1), (2, state2) ]