{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           "haskeline" System.Console.Haskeline

import qualified "containers" Data.Map as M

import           "base" Control.Monad
import           "base" Data.IORef
import           "base" Data.List
import           "base" Data.Function
import           "base" Data.Foldable
import           "base" Data.Maybe
import           "base" Data.Char
import           "base" System.Exit

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
          , whatIs "like" "I like" 
          , whatIs "zip code" "my zip code is" 
          , whatIs "phone number" "my phone number is" 
          , whatIs "country" "my country is"
          , whatIs "drink" "my favorite drink is"
          , whatIs "car" "my favorite car is" 
          ,whatIs "class" "my favorite class or subject is" 
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
                   
                    , Option { key = "like" 
                   , command = "I like" 
                   , question = "what do I like"
                   , answer = "You like" 
                   }
                   
          , Option { key = "zip code" 
                   , command = "I have the zip code"  
                   , question = "what is my zip code"
                   , answer = "Your zip code"
                   } 
                   
          , Option { key = "phone number" 
                   , command = "I have the phone number"  
                   , question = "what is my phone number"
                   , answer = "Your phone number is"
                   }
                   
          , Option { key = "country" 
                   , command = "I live in the" 
                   , question = "what country do I live in" 
                   , answer = "Your country is"
                   }
                   
          , Option { key = "drink" 
                   , command = "my favorite drink is"  
                   , question = "what is my favorite drink"
                   , answer = "Your favorite drink is"
                   } 
                   
          , Option { key = "car" 
                   , command = "my favorite car is" 
                   , question = "what is my favorite car" 
                   , answer = "Yur favorite car is"
                   }
                   
          , Option { key = "class" 
                   , command = "my favorite class is" 
                   , question = "what is my favorite class" 
                   , answer = "Your favorite class is"
                   }
          ]

optionToActions :: Option -> [Action]
optionToActions Option{..} =
  [ (command , \val ->  ChangeValue $ M.insert key (Value val ""))
  , (question, \_   ->  Say $ \vals -> 
                                case M.lookup key vals of
                                    Just Value{object} -> answer ++ " " ++ object
                                    Nothing  -> "I don't know your " ++ key
    )
  ]


sanitize :: String -> String
sanitize s = map toLower s & trim

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

type Command = String
type Result = String
type Answer = String
type Key = String
data Value = Value { object :: String, reason :: String }
type Values = M.Map Key Value
data Effect = ChangeValue (Values -> Values) | Say (Values -> Answer)
type Action = (Command, Result -> Effect)

specialActions:: [Action]
specialActions =
 [ ( "my"
   , \cmd -> case words cmd of --NOTE changed to this because a pattern-match should be total
       (x:"is":y:"because":zs) -> ChangeValue $ M.insert x (Value y (unwords zs))
       (x:"is":[y])            -> ChangeValue $ M.insert x (Value y "")
       _                       -> Say $ const "Did you mean 'my _ is _' or 'my _ is _ because _'?"
   )

 , ( "what is my"
   , \key -> Say $ \vals ->
        case M.lookup key vals of
          Just Value{..} -> "Your " ++ key ++ " is " ++ object
          Nothing  -> "I don't know your " ++ key
   )

 , ( "why is my"
   , whyIsMy
   )
 , commandResponse "tell me a joke" "I hate Russian dolls, they're so full of themselves."
 
 , ( "why is my" 
    , whyIsMy 
   )
 , commandResponse "tell me another joke" "Velcro - what a rip-off!" 
 ]

commandResponse command response = (command, const $ Say $ const response)

whyIsMy suffix = case words suffix of
    [key, property] -> Say $ \vals ->
        case M.lookup key vals of
          Just Value{..} ->
            if object == property
            then intercalate " " ["Your", key, "is", object, "because", reason]
            else intercalate " " ["Your", key, "is not", property]
          Nothing ->
            intercalate " " ["I don't know why your", key, "is", property]
    _ -> Say $ const $ intercalate " " ["I don't know why your", suffix, "is the way it is"]
 
actions :: [Action]
actions = (map optionToActions options & concat)
  ++ specialActions

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
main = do 
    start
    runInputT defaultSettings (loop M.empty)
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
                                
start :: IO () 
start = do 
    putStrLn "Hello"
    putStrLn "Welcome to Haskell Chat Bot, user created by fuskerbrothers"
    putStrLn "You will now be taken to the program, Thanks for using our software" 
    putStrLn "if you wish to continue the program press anything if you wish to quit it type yes and then press enter" 
    line <- getLine
    when (line == "yes") exitSuccess
    putStrLn "Lets start the program the instructions will come with this programs download" 