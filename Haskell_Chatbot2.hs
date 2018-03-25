import Control.Monad
import Data.IORef
import qualified Data.Map as M


main :: IO ()
main = do
  putStrLn "Enter commands"
  store <- newIORef M.empty
  forever $ do
    cmd <- getLine
    case words cmd of
      ["Call", "me", name] -> do --code from lines 15-60 is the code for the AI 
        putStrLn $ "Hi " ++ name
        modifyIORef store $ M.insert "name" name
      ["What","is","my","name?"] -> do
        m <- readIORef store
        case M.lookup "name" m of
          Nothing -> putStrLn "I don't know your name."
          Just name -> putStrLn $ "Your name is " ++ name
      ["I", "am", age] -> do
        putStrLn $ "You are " ++ age ++ " years old."
        modifyIORef store $ M.insert "age" age
      ["I","live","in","the",country] -> do 
        putStrLn $ "You live in " ++ country ++ "!"
      ["I","go","to",school] -> do 
        putStrLn $ "You go to " ++ school ++ "!"
        modifyIORef store $M.insert "school" school 
      ["What","school","do","I","go","to"] -> do 
        m <- readIORef store 
        case M.lookup "school" m of 
            Nothing -> putStrLn "I don't know what school you go to."
            Just school -> putStrLn $ "Your school is " ++school 
      ["I","get",grades] -> do 
        putStrLn $ "You get " ++ grades ++ "!"
        modifyIORef store $M.insert "grades" grades
      ["What","are","my","grades"] -> do 
        m <- readIORef store 
        case M.lookup "grades" m of 
            Nothing -> putStrLn "I don't know what grades you get."
            Just grades -> putStrLn $ "Your grades are " ++grades
      ["I","live","in",city] -> do 
        putStrLn $ "You live in " ++ city 
        modifyIORef store $ M.insert "city" city
        -- key: "city"         (this is a constant string)
        -- value: city         (this is a variable)
        -- "city" is 'city'.
      ["What","is","my","city?"] -> do
        m <- readIORef store
        case M.lookup "city" m of 
          Nothing -> putStrLn "I don't know your city."
          Just city -> putStrLn $ "Your city is " ++ city 
{-
For this first enter you're variable then an adjective || then ask what is you're variable
-}
      ["what", "is", x] -> do
        m <- readIORef store
        case M.lookup x m of
          Nothing -> putStrLn $ "I don't know about " ++ x
          Just y  -> putStrLn $ "So " ++ x ++ " is equal to " ++ y 
      [x, "is", y] -> do
        modifyIORef store $ M.insert x y
      _ -> putStrLn "I don't understand you."