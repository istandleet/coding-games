{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
import System.IO
import Control.Monad
import Control.Applicative
import Data.Function
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    board <- getBoard
    
    -- game loop
    forever $ do
        turnInput <- getTurnInput
        
        hPutStrLn stderr "Debug messages..."
        hPrint stderr turnInput
        mapM_ (hPrint stderr) $ map ((,) <$> id <*> preferenceRank board turnInput) (possibleActions turnInput)

        -- GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message>
        
        print $ chooseAction board turnInput

type Board = Map CellIndex Cell
mkBoard :: [Cell] -> Board
mkBoard = Map.fromList . map ((,) <$> cellIndex <*> id)
type CellIndex = Int
data Cell = Cell
   { cellIndex :: !CellIndex
   , richness :: !Int
   , neighbors :: ![CellIndex]
   } deriving (Eq, Ord, Show)

data Action = 
     WAIT 
   | COMPLETE !Int
   | GROW !Int
   | SEED !Int !Int
   deriving (Eq, Read, Ord, Show)

data TurnInput = TurnInput
   { day :: !Int
   , nutrients :: !Int
   , myTurnState :: !MyTurnState
   , oppTurnState :: !OppTurnState
   , trees :: Map CellIndex TreeState
   , possibleActions :: [Action]
   } deriving (Eq, Ord, Show)

data MyTurnState = MyTurnState
   { mySun :: !Int
   , myScore :: !Int
   } deriving (Eq, Ord, Show)

data OppTurnState = OppTurnState
   { oppSun :: !Int
   , oppScore :: !Int
   , oppIsWaiting :: !Bool
   } deriving (Eq, Ord, Show)

data TreeState = TreeState
   { size :: !Int
   , isMine :: !Bool
   , isDormant :: !Bool
   } deriving (Eq, Ord, Show)

chooseAction :: Board -> TurnInput -> Action
chooseAction board input = Data.List.maximumBy (compare `on` preferenceRank board input) (possibleActions input)
    
preferenceRank :: Board -> TurnInput -> Action -> Maybe Int
preferenceRank board input = \case
    WAIT -> Nothing
    COMPLETE cellindex -> if day input == 5 then scoreComplete board input cellindex else Nothing
    GROW cellindex -> liftA2 (\s r -> 3*s+r) (size <$> Map.lookup cellindex (trees input)) (richness <$> Map.lookup cellindex board)

score :: Board -> TurnInput -> Action -> Maybe Int
score board ti@TurnInput{..} action = do
    actionCost <- cost ti action
    guard $ actionCost <= mySun myTurnState
    case action of 
        WAIT -> Just $ if day == 5 then mySun myTurnState `div` 3 else 0
        COMPLETE cellindex -> scoreComplete board ti cellindex
        GROW _ -> Just 0

scoreComplete :: Board -> TurnInput -> CellIndex -> Maybe Int
scoreComplete board TurnInput{..} cellindex = do
    tree <- Map.lookup cellindex trees 
    guard $ size tree == 3
    cell <- Map.lookup cellindex board
    return $ (2*richness cell) + nutrients

cost :: TurnInput -> Action -> Maybe Int
cost ti@TurnInput{..} = \case 
    WAIT -> Just 0
    COMPLETE cellindex -> do
        tree <- Map.lookup cellindex trees
        return 4
    GROW cellindex -> do
        tree <- Map.lookup cellindex trees
        case size tree of
            1 -> pure $ 3 + mySize2 ti
            2 -> pure $ 7 + mySize3 ti
            _ -> Nothing

mySize2, mySize3 :: TurnInput -> Int
mySize2 = length . Map.filter (\TreeState{..} -> isMine && size == 2) . trees
mySize3 = length . Map.filter (\TreeState{..} -> isMine && size == 3) . trees

-- ########################## Input Functions ##########################
getBoard :: IO Board
getBoard = do
    input_line <- getLine
    let numberofcells = read input_line :: Int -- 37
    cells <- replicateM numberofcells $ do
        input_line <- getLine
        let input = words input_line
        let cellIndex = read (input!!0) :: Int -- 0 is the center cell, the next cells spiral outwards
        let richness = read (input!!1) :: Int -- 0 if the cell is unusable, 1-3 for usable cells
        let neighbors = map read $ drop 2 input
        return Cell{..}
    return $ mkBoard cells

getTurnInput :: IO TurnInput
getTurnInput = do
    input_line <- getLine
    let day = read input_line :: Int -- the game lasts 24 days: 0-23
    input_line <- getLine
    let nutrients = read input_line :: Int -- the base score you gain from the next COMPLETE action
    
    input_line <- getLine
    let input = words input_line
        mySun = read (input!!0) :: Int -- your sun points
        myScore = read (input!!1) :: Int -- your current score
        myTurnState = MyTurnState{..}

    input_line <- getLine
    let input = words input_line
        oppSun = read (input!!0) :: Int -- opponent's sun points
        oppScore = read (input!!1) :: Int -- opponent's score
        oppIsWaiting = read (input!!2) == 1 :: Bool -- whether your opponent is asleep until the next day
        oppTurnState = OppTurnState{..}

    input_line <- getLine
    let numberoftrees = read input_line :: Int -- the current amount of trees
    trees <- fmap Map.fromList $ replicateM numberoftrees $ do
        input_line <- getLine
        let input = words input_line
        let cellindex = read (input!!0) :: Int -- location of this tree
        let size = read (input!!1) :: Int -- size of this tree: 0-3
        let isMine = read (input!!2) == 1 :: Bool -- 1 if this is your tree
        let isDormant = read (input!!3) == 1 :: Bool -- 1 if this tree is dormant
        return (cellindex, TreeState{..})

    input_line <- getLine
    let numberofpossibleactions = read input_line :: Int -- all legal actions
    possibleActions <- map read <$> replicateM numberofpossibleactions getLine
    
    return TurnInput{..}
