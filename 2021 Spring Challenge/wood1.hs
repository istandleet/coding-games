{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
import System.IO
import Control.Monad
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

-- ########################## Basic Mechanics ##########################

type Board = Map CellIndex Richness -- todo needs neighbors
type CellIndex = Int
type Richness = Int
mkBoard :: [CellInput] -> Board
mkBoard = Map.fromList . map ((,) <$> ci_cellIndex <*> ci_richness)
getRichness :: Board -> CellIndex -> Richness -- assume you are actually using a CellIndex
getRichness board cellindex = board Map.! cellindex

data Action = 
     WAIT 
   | COMPLETE !Int
   | GROW !Int
   | SEED !Int !Int
   deriving (Eq, Read, Ord, Show)

-- | The minimal version of the game fed to AI
data SymState = SymState
   { day :: !Int
   , nutrients :: !Int
   , p1State :: !PlayerState -- player
   , p2State :: !PlayerState -- opponent
   } deriving (Eq, Ord, Show)

data GameState = GameState
   { day :: !Int
   , nutrients :: !Int
   , p1State :: !PlayerState -- player
   , p2State :: !PlayerState -- opponent
   } deriving (Eq, Ord, Show)

data PlayerState = PlayerState
   { points :: !Int
   , sunPoints :: !Int
   , isWaiting :: !Bool
   , trees :: [Tree]
   } deriving (Eq, Ord, Show)
getTree :: PlayerState -> CellIndex -> Maybe Tree
getTree ps i = Data.List.find ((==i) . cellIndex) (trees ps)

data Tree = Tree
   { cellIndex :: !CellIndex
   , size :: !Int
   , isDormant :: !Bool
   } deriving (Eq, Ord, Show)

mkTree :: TreeInput -> Tree
mkTree TreeInput{..} = Tree
    { cellIndex = ti_cellindex
    , size = ti_size
    , isDormant = ti_isDormant
    }

mkGameState :: TurnInput -> GameState
mkGameState TurnInput{..} = GameState
    { day = i_day
    , nutrients = i_nutrients
    , p1State = PlayerState
        { points = myScore myTurnState
        , sunPoints = mySun myTurnState
        , isWaiting = False
        , trees = map mkTree mytrees
        } 
    , p2State = PlayerState
        { points = oppScore oppTurnState
        , sunPoints = oppSun oppTurnState
        , isWaiting = oppIsWaiting oppTurnState
        , trees = map mkTree opptrees
        } 
    }
    where
    (mytrees, opptrees) = Data.List.partition ti_isMine i_trees
{-
score :: Board -> GameState -> Action -> Maybe Int
score board gs@GameState{..} action = do
    actionCost <- cost gs action
    guard $ actionCost <= mySun myTurnState
    case action of 
        WAIT -> Just $ if day == 5 then mySun myTurnState `div` 3 else 0
        COMPLETE cellindex -> scoreComplete board ti cellindex
        GROW _ -> Just 0

scoreComplete :: Board -> TurnInput -> CellIndex -> Maybe Int
scoreComplete board TurnInput{..} cellindex = do
    tree <- Map.lookup cellindex trees 
    guard $ size tree == 3
    return $ (2*getRichness board cellindex) + nutrients
-}
cost :: PlayerState -> Action -> Maybe Int
cost ps = \case 
    WAIT -> Just 0
    COMPLETE cellindex -> do
        tree <- getTree ps cellindex
        return 4
    GROW cellindex -> do
        tree <- getTree ps cellindex
        case size tree of
            1 -> pure $ 3 + length (filter ((==2) . size) (trees ps))
            2 -> pure $ 7 + length (filter ((==3) . size) (trees ps))
            _ -> Nothing

-- ########################## AI ##########################

chooseAction :: Board -> TurnInput -> Action
chooseAction board input = Data.List.maximumBy (compare `on` preferenceRank board input) (possibleActions input)

preferenceRank :: Board -> TurnInput -> Action -> Maybe Int
preferenceRank board input = \case
    WAIT -> Nothing
    COMPLETE cellindex -> scoreComplete board input cellindex
    GROW cellindex -> size <$> Map.lookup cellindex (trees input)

-- ########################## Input Functions ##########################
data CellInput = CellInput
   { ci_cellIndex :: !CellIndex
   , ci_richness :: !Int
   , ci_neighbors :: ![CellIndex]
   } deriving (Eq, Ord, Show)

data TurnInput = TurnInput
   { i_day :: !Int
   , i_nutrients :: !Int
   , myTurnState :: !MyTurnState
   , oppTurnState :: !OppTurnState
   , i_trees :: [TreeInput]
   , i_possibleActions :: [Action]
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

data TreeInput = TreeInput
   { ti_cellindex :: !CellIndex
   , ti_size :: !Int
   , ti_isMine :: !Bool
   , ti_isDormant :: !Bool
   } deriving (Eq, Ord, Show)

getBoard :: IO Board
getBoard = do
    input_line <- getLine
    let numberofcells = read input_line :: Int -- 37
    cells <- replicateM numberofcells $ do
        input_line <- getLine
        let input = words input_line
        let ci_cellIndex = read (input!!0) :: Int -- 0 is the center cell, the next cells spiral outwards
        let ci_richness = read (input!!1) :: Int -- 0 if the cell is unusable, 1-3 for usable cells
        let ci_neighbors = map read $ drop 2 input
        return CellInput{..}
    return $ mkBoard cells

getTurnInput :: IO TurnInput
getTurnInput = do
    input_line <- getLine
    let i_day = read input_line :: Int -- the game lasts 24 days: 0-23
    input_line <- getLine
    let i_nutrients = read input_line :: Int -- the base score you gain from the next COMPLETE action
    
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
    i_trees <- replicateM numberoftrees $ do
        input_line <- getLine
        let input = words input_line
        let ti_cellindex = read (input!!0) :: Int -- location of this tree
        let ti_size = read (input!!1) :: Int -- size of this tree: 0-3
        let ti_isMine = read (input!!2) == 1 :: Bool -- 1 if this is your tree
        let ti_isDormant = read (input!!3) == 1 :: Bool -- 1 if this tree is dormant
        return TreeInput{..}

    input_line <- getLine
    let numberofpossibleactions = read input_line :: Int -- all legal actions
    i_possibleActions <- map read <$> replicateM numberofpossibleactions getLine
    
    return TurnInput{..}
