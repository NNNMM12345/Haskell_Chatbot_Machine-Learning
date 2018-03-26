module Main where

import Control.Monad
import Data.IORef
import System.Console.Haskeline
import qualified Data.Map as M
import Data.List
import Data.Function
import Data.Foldable
import Data.Maybe
import Data.Char

data Option = Option { key :: String
                     , command :: String
                     , question :: String
                     , answer :: String
                     }

whatIs key command = Option { key = key
                            , command = command
                            , question = "what is my " ++ key
                            , answer = "Your " ++ key ++ " is"
                            }

options :: [Option]
options = [ whatIs "name" "Call me"
          , whatIs "age" "I am"
          , whatIs "city" "I live in"
          , Option { key = "school" 
                   , command = "I go to"  
                   , question = "what school do I go to"
                   , answer = "Your school is"
                   }

          , Option { key = "grades" 
                   , command = "I get"    
                   , question = "what are my grades"
                   , answer = "Your grades are"
                   }
          ]

optionToActions :: Option -> [Action]
optionToActions Option {key=key, command=command, question=question, answer=answer} =
  [ (command , \val ->  ChangeValue $ M.insert key val)
  , (question, \_   ->  Say $ \vals -> 
                                case M.lookup key vals of
                                    Just val -> answer ++ " " ++ val
                                    Nothing  -> "I don't know your " ++ key
    )
  ]

sanitize :: String -> String
sanitize s = map toLower s & trim

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

type Command = String
type Answer = String
type Key = String
type Value = String
type Values = M.Map Key Value
data Effect = ChangeValue (Values -> Values) | Say (Values -> Answer)
type Action = (Command, Value -> Effect)

actions :: [Action]
actions = (map optionToActions options & concat)
              ++ [ ("my", \val -> let [x,"is",y] = words val
                                  in ChangeValue $ M.insert x y)
                 , ("what is my", \key -> Say $ \vals -> 
                                                  case M.lookup key vals of
                                                      Just val ->  "Your " ++ key ++ " is " ++ val
                                                      Nothing  -> "I don't know your " ++ key
                   )
                 ]

ai :: Command -> Effect
ai request =
  actions & map applyCommand
          & asum
          & (fromMaybe $ Say (\_ -> "I don't understand you") :: Maybe Effect -> Effect)
  where
    applyCommand :: Action -> Maybe Effect
    applyCommand (command, val2effect) =
        sanitize command `stripPrefix` sanitize request
        & fmap (val2effect . sanitize)



main :: IO ()
main = runInputT defaultSettings (loop M.empty)
   where
       loop values = do
           minput <- getInputLine ">> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> case ai input of
                               ChangeValue changer -> loop (changer values)
                               Say answer -> do
                                outputStrLn (answer values)
                                loop values