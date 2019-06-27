{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Arrow      ((&&&))
import           Data.Machine.Mealy
import           Data.Machine.Moore
import           Data.Profunctor    (rmap)

data Input
  = Zero
  | One

parseInput :: String -> Maybe Input
parseInput = \case
  "0" -> Just Zero
  "1" -> Just One
  _   -> Nothing

data Output
  = P
  | Q
  | R
  deriving (Show)

myMachine :: Moore Input Output
myMachine = unfoldMoore transitions P
  where
    transitions :: Output -> (Output, Input -> Output)
    transitions = id &&& update

    update :: Output -> Input -> Output
    update = \case
      P -> \case
        Zero -> Q
        One  -> R
      Q -> \case
        Zero -> Q
        One  -> R
      R -> \case
        Zero -> R
        One  -> Q

showMachine :: Moore Input String
showMachine = rmap show myMachine

-- | "a" is the state of the console
-- | "b" is the state passed to the event handler
newtype ConsoleUI a b = ConsoleUI { runUI :: (b -> IO ()) -> a -> IO ()}

interactor :: (a -> Moore a b) -> (b -> IO ()) -> a -> IO ()
interactor transitions printer state = do
  case (transitions state) of
    (Moore out' _) -> do
      printer out'

natTrans :: forall a b . Moore a b -> ConsoleUI a b
natTrans (Moore _ transitions) = ConsoleUI (interactor transitions)

animate :: ConsoleUI a a -> a -> IO ()
animate ui state = do
  runUI ui (animate ui) state

-- | explicit version of loop
loop :: Show state => Moore Input state -> IO ()
loop machine@(Moore s transitions) = do
  putStrLn $ "Your current state is " <> show s <> ". Please provide an input" -- provide information on the current state
  inputString <- getLine                                                       -- collect input
  case parseInput inputString of
    Nothing -> do
      putStrLn "Not a valid input. Please try again"
      loop machine
    Just i -> loop $ transitions i                                             -- compute next state from current state and input

-- | start abstracting on IO operations
-- | we abstract an operation to show the state
-- | and an operation to collect input
loop' :: Show state => Moore input state -> (state -> IO ()) -> IO input -> IO ()
loop' (Moore s transitions) showState collectInput = do
  showState s                                  -- provide information on the current state
  i <- collectInput                            -- collect input
  loop' (transitions i) showState collectInput -- compute next state from current state and input

retrieveInputFromConsole :: IO Input
retrieveInputFromConsole = do
  putStrLn "Please provide an input"
  inputString <- getLine
  case parseInput inputString of
    Nothing -> do
      putStrLn "Not a valid input. Please try again"
      retrieveInputFromConsole
    Just i -> pure i

class Processor p where
  currentState :: p a b -> b
  next :: p a b -> a -> p a b

instance Processor Moore where
  currentState (Moore s _) = s
  next (Moore _ t) s       = t s

-- | try to abstract over the Moore machine (we don't go far, though)
loop'' :: (Show state, Processor p) => p input state -> (state -> IO ()) -> IO input -> IO ()
loop'' processor showState collectInput = do
  showState (currentState processor)               -- provide information on the current state
  i <- collectInput                                -- collect input
  loop'' (next processor i) showState collectInput -- compute next state from current state and input

-- | abstract over the monad where the computation takes place
loop''' :: (Show state, Processor p, Monad m) => p input state -> (state -> m a) -> m input -> m a
loop''' processor processState collectInput = do
  a <- processState (currentState processor)           -- provide information on the current state
  i <- collectInput                                    -- collect input
  loop''' (next processor i) processState collectInput -- compute next state from current state and input

mainMoore :: IO ()
mainMoore = loop''' myMachine
  (\s -> putStrLn $ "Your current state is " <> show s)
  retrieveInputFromConsole

-- | every Moore machine can be seen as a Mealy machine
-- | so we can use profunctor optics which are available for Mealy but not for Moore machines
-- | this creates a delay between the inputs and outputs
-- | the output is the result of the previous step
mooreToMealy :: Moore a b -> Mealy a b
mooreToMealy (Moore status next) = Mealy $ const status &&& (mooreToMealy . next)

loopMealy :: (Show output, Monad m) => Mealy input output -> (output -> m a) -> m input -> m a
loopMealy (Mealy next) processState collectInput = do
  i <- collectInput
  let (newState, newMachine) = next i
  _ <- processState newState
  loopMealy newMachine processState collectInput

mainMealy :: IO ()
mainMealy = loopMealy
  (mooreToMealy myMachine)
  (\s -> putStrLn $ "You just left " <> show s)
  retrieveInputFromConsole

main :: IO ()
main = mainMealy
