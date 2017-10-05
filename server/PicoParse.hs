module PicoParse
( parseRules
) where

import qualified Prelude
import Prelude hiding (Left, Right)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad (void)
import Data.List (groupBy, sortBy, find)
import Data.Function (on)

import PicoEngine

main :: IO ()
main = do
    let str = "0 Ux** -> N 0\n0 UR** -> N 0\n0 xx** -> D 0\n"
        s = Parsec.runParser states [] "(source)" str
    print s

parseRules :: String -> Either String [(Int, State)]
parseRules str = case Parsec.runParser states [] "(source)" str of
                    Prelude.Left  e -> Prelude.Left (show e)
                    Prelude.Right s -> Prelude.Right s

ruleExists :: (Int, Rule) -> [(Int, Rule)] -> Maybe (Int, Rule)
ruleExists r = find (ruleEq r) -- Eta reduced

ruleEq :: (Int, Rule) -> (Int, Rule) -> Bool
ruleEq a b = let a' = snd a
                 b' = snd b
                 s1 = fst a
                 s2 = fst b 
              in s1 == s2
                 && matchEq (up a') (up b')
                 && matchEq (left a') (left b')
                 && matchEq (down a') (down b')
                 && matchEq (right a') (right b')
    where matchEq Blocked Empty = False
          matchEq Empty Blocked = False
          matchEq _ _ = True

group :: [(Int, b)] -> [[(Int, b)]]
group = groupBy ((==) `on` fst)

sort :: [(Int, b)] -> [(Int, b)]
sort = sortBy (compare `on` fst)

tupleCat :: [(a, b)] -> (a, [b]) -> (a, [b])
tupleCat xs t = foldl (\t' x -> (fst x, snd x : snd t')) t xs

groupStates :: [[(Int, Rule)]] -> [(Int, State)] -> [(Int, State)]
groupStates xs s = foldr (\x -> (:) (tupleCat x (0, []))) s xs

states :: Parsec.Parsec String [(Int, Rule)] [(Int, State)]
states = do
    rules <- Parsec.many1 rule
    let grouped = group $ sort rules
    return $ groupStates grouped []

rule :: Parsec.Parsec String [(Int, Rule)] (Int, Rule)
rule = do
    state <- readInt <$> Parsec.many1 Parsec.digit
    Parsec.space
    up'    <- readMatch <$> Parsec.oneOf "*xU" <?> "*, x or U"
    right' <- readMatch <$> Parsec.oneOf "*xR" <?> "*, x or R"
    down'  <- readMatch <$> Parsec.oneOf "*xD" <?> "*, x or D"
    left'  <- readMatch <$> Parsec.oneOf "*xL" <?> "*, x or L"
    Parsec.space
    Parsec.string "->"
    Parsec.space
    action' <- readAction <$> Parsec.oneOf "ULDRN" <?> "U, L, D, R or N"
    Parsec.space
    next' <- readInt <$> Parsec.many1 Parsec.digit
    Parsec.manyTill Parsec.space (Parsec.eof <|> void (Parsec.char '\n'))

    existingRules <- Parsec.getState
    let newRule = Rule { up = up', right = right', down = down', left = left', 
                         action = action', next = next' }
        dup     = ruleExists (state, newRule) existingRules

    case dup of
        Nothing -> Parsec.putState ((state, newRule) : existingRules) >> return (state, newRule)
        Just e  -> fail $ "Repeat rule detected\n" ++ repeatErr (state, newRule) e

repeatErr :: (Int, Rule) -> (Int, Rule) -> String
repeatErr (s1, r1) (s2, r2) = "Rule: " ++ show s1 ++ " " ++ showRule r1 ++ " overlaps with\n" 
                           ++ "Rule: " ++ show s2 ++ " " ++ showRule r2

showRule :: Rule -> String
showRule rr = let u = dispMatch (up rr) "U"
                  r = dispMatch (right rr) "R"
                  d = dispMatch (down rr) "D"
                  l = dispMatch (left rr) "L"
                  a = dispAction (action rr)
                  n = show $ next rr
              in u ++ r ++ d ++ l ++ " -> " ++ a ++ " " ++ n
        where dispMatch m c = case m of
                                Always -> "*"
                                Empty  -> "x"
                                Blocked -> c 
              dispAction a  = case a of
                                Left -> "L"
                                Right -> "R"
                                Up -> "U"
                                Down -> "D"
                                None -> "N"

readInt :: String -> Int
readInt = read :: String -> Int

readMatch :: Char -> Match
readMatch c = case c of
                '*' -> Always
                'x' -> Empty
                _   -> Blocked

readAction :: Char -> Action
readAction c = case c of
                 'U' -> Up
                 'L' -> Left
                 'D' -> Down
                 'R' -> Right
                 'N' -> None
