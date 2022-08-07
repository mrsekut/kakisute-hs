module Bell where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Data.Map as Map
import Control.Concurrent.MVar (newMVar)


-- newtype Logger = Logger (MVar LogCommand)
-- data LogCommand = Message String | Stop (MVar ())

-- main :: IO ()
-- main = do
--   l <- initLogger
--   logMessage l "hello"
--   logMessage l "bye"
--   logStop l


-- initLogger :: IO Logger
-- initLogger = do
--   m <- newEmptyMVar
--   let l = Logger m
--   forkIO $ logger l
--   return l


-- logger :: Logger -> IO ()
-- logger (Logger m) = loop
--   where
--     loop = do
--       cmd <- takeMVar m
--       case cmd of
--         Message s -> do
--           putStrLn s
--           loop
--         Stop s -> do
--           putStrLn "logger: stop"
--           putMVar s ()


-- logMessage :: Logger -> String -> IO ()
-- logMessage (Logger m) s = putMVar m (Message s)


-- logStop :: Logger -> IO ()
-- logStop (Logger m) = do
--   s <- newEmptyMVar
--   putMVar m (Stop s)
--   takeMVar s

type Name = String
type PhoneNumber = String
type PhoneBook = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

main = do
  s <- new
  sequence_ [ insert' s ("name" ++ show n) (show n) | n <- [1..10000] ]
  lookup' s "name999" >>= print
  lookup' s "unknown" >>= print

new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return $ PhoneBookState m


insert' :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert' (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m (Map.insert name number book)


lookup' :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup' (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return $ Map.lookup name book
