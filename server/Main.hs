{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import PicoEngine
import PicoParse
import Data.Text.Lazy (pack)

main :: IO ()
main = scotty 3000 $
    post "/" $ do
        rules <- param "rules"
        case parseRules rules of
            Prelude.Left e       -> text $ pack e
            Prelude.Right states -> text $ pack $ show $ runGame defaultBoard defaultPico states 1000


--runGame defaultBoard defaultPico states

--runGame :: Board -> Pico -> [(Int, State)] -> Int -> GameState
--parseRules :: String -> Either String [(Int, State)]

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