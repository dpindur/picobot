--0123456789101112
--0 x*** -> N   0

import Html exposing (..)
import Result
import Graphics.Element exposing (..)
import String exposing (uncons)
import Maybe


type Token = State | Direction | Action

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
                              (Just s, rest) -> Ok (token ++ [State], rest)

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
                            Just (h,s) -> if | h == 'x'  -> Ok (token ++ [Direction], s)
                                             | h == '*'  -> Ok (token ++ [Direction], s)
                                             | h == 'U'  -> Ok (token ++ [Direction], s)
                                             | h == 'D'  -> Ok (token ++ [Direction], s)
                                             | h == 'L'  -> Ok (token ++ [Direction], s)
                                             | h == 'R'  -> Ok (token ++ [Direction], s)
                                             | otherwise -> Err "Error on direction"
                    
action : (List Token, String) -> Result String (List Token, String)
action a = let (token, xs) = a
              in  case xs of
                    "" -> Err "Error on action"
                    x  -> case String.uncons x of
                            Nothing    -> Err "Error on action"
                            Just (h,s) -> if | h == 'U'  -> Ok (token ++ [Action], s)
                                             | h == 'D'  -> Ok (token ++ [Action], s)
                                             | h == 'L'  -> Ok (token ++ [Action], s)
                                             | h == 'R'  -> Ok (token ++ [Action], s)
                                             | h == 'S'  -> Ok (token ++ [Action], s)
                                             | otherwise -> Err "Error on action"


test : (List Token, String) -> Result String (List Token, String)
test a = let (token, xs) = a
              in  case xs of
                    "" -> Err "Error on Test"
                    _  -> Ok (token ++ [Direction], xs)             


                                    
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
                   

main = (show (rule ("123 **** -> S 123")))
