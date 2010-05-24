-- | This module defines the HFiaR monad and all the actions you can perform in it
module HFiaR (
-- * MonadTrans controls
    HFiaRT, play, eval,
-- * Monad controls
    HFiaR, justPlay, justEval,
-- * Types
    Game, Player(..), Tile(..), HFiaRError(..), HFiaRResult(..),
-- * Actions
    dropIn, player, board, result
    ) where

import Control.Monad.State

-- | Posible errors in the HFiaR Monad
data HFiaRError = GameEnded | GameNotEnded | InvalidColumn | FullColumn
    deriving (Eq)
    
instance Show HFiaRError where
    show GameEnded      = "Game ended"
    show GameNotEnded   = "Game is on course yet"
    show InvalidColumn  = "That column doesn't exist"
    show FullColumn     = "That column is full"

-- | Posible tiles (just green or red ones)
data Tile = Red | Green
    deriving (Eq, Show)

-- | Posible players (each one with his tile colour)
data Player = Pl {tiles :: Tile}
    deriving (Eq)
    
instance Show Player where
    show (Pl t) = show t

-- | Posible results for the game
data HFiaRResult = Tie | WonBy Player
    deriving (Eq, Show)

-- | Game description
data Game = OnCourse {gamePlayer :: Player,
                      gameBoard  :: [[Tile]]} |
            Ended {gameResult :: HFiaRResult,
                   gameBoard  :: [[Tile]]}
    deriving (Eq, Show)
    
-- | Generic HFiaRT type
newtype HFiaRT m a = HFT {state :: StateT Game m a}
    deriving (Monad, MonadIO, MonadTrans)
    
instance Monad m => MonadState Game (HFiaRT m) where
    get = HFT $ get
    put = HFT . put

-- | Basic HFiaR type - ready to /just/ play HFiaR actions
type HFiaR = HFiaRT IO

-- | Starts a game, run the /HFiaRT/ actions and returns the game
justPlay :: HFiaR a -> IO Game
justPlay actions = play actions 

-- | Starts a game, run the /HFiaRT/ actions and returns the result of the last one
justEval :: HFiaR a -> IO a
justEval actions = eval actions

-- | Starts a game, run the /HFiaRT/ actions and returns the game wrapped up in the /m/ monad
play :: Monad m => HFiaRT m a -> m Game
play actions = (state actions) `execStateT` (OnCourse (Pl Green) (replicate 7 []))

-- | Starts a game, run the /HFiaRT/ actions and returns the result of the last one wrapped up in the /m/ monad
eval :: Monad m => HFiaRT m a -> m a
eval actions = (state actions) `evalStateT` (OnCourse (Pl Green) (replicate 7 []))

--------------------------------------------------------------------------------
-- | Drop a tile in a column
dropIn :: Monad m => Int -- ^ Column number
       -> HFiaRT m (Either HFiaRError ())
dropIn c | c < 0 = return $ Left InvalidColumn
         | 6 < c = return $ Left InvalidColumn
         | otherwise =
            do
                game <- get
                case game of
                    Ended{} -> return $ Left GameEnded
                    OnCourse{gameBoard = theBoard,
                             gamePlayer= thePlayer} ->
                        if length (theBoard !! c) == 7
                            then return $ Left FullColumn
                            else
                                let newBoard = insertAt c (tiles thePlayer) theBoard
                                    newResult= if (isWinner c thePlayer newBoard) then WonBy thePlayer else Tie
                                    newGame  = if (full newBoard || (newResult == WonBy thePlayer))
                                                   then Ended{gameResult = newResult,
                                                              gameBoard  = newBoard}
                                                   else OnCourse{gameBoard = newBoard,
                                                                 gamePlayer= otherPlayer thePlayer}
                                 in put newGame >>= return . Right
    where insertAt :: Int -> a -> [[a]] -> [[a]]
          insertAt i x xss = (take i xss) ++ ( (x : (xss !! i)) : drop (i+1) xss)
          
          otherPlayer :: Player -> Player
          otherPlayer Pl{tiles=Green} = Pl Red
          otherPlayer Pl{tiles=Red} = Pl Green
          
          full :: [[a]] -> Bool
          full = all (\x -> 7 == length x)
          
          isWinner :: Int -> Player -> [[Tile]] -> Bool
          isWinner cc Pl{tiles=p} b =
            let col = b !! cc
             in ([p,p,p,p] == take 4 col) ||
                fourIn (getRow (length col - 1) b) ||
                fourIn (getDiagUpRight cc (length col - 1) b) ||
                fourIn (getDiagUpLeft  cc (length col - 1) b)

          getRow :: Int -> [[Tile]] -> [Maybe Tile]
          getRow r = map (cell r)
          
          getDiagUpRight :: Int -> Int -> [[Tile]] -> [Maybe Tile]
          getDiagUpRight cc r xss = map (\i -> cell (i+r-cc) (xss !! i)) [0..6]
          
          getDiagUpLeft :: Int -> Int -> [[Tile]] -> [Maybe Tile]
          getDiagUpLeft cc r xss = map (\i -> cell (r+cc-i) (xss !! i)) [0..6]
          
          cell :: Int -> [Tile] -> Maybe Tile
          cell cc xs = if (cc >= 0 && cc < length xs)
                        then Just $ (reverse xs) !! cc
                        else Nothing

          fourIn :: [Maybe Tile] -> Bool
          fourIn [] = False
          fourIn (Nothing:xs) = fourIn xs
          fourIn (Just p :xs) = ([Just p, Just p, Just p] == take 3 xs) || fourIn xs

-- | Player who's supposed to play the next tile
player :: Monad m => HFiaRT m (Either HFiaRError Player)
player = get >>= \game -> return $ case game of
                                        Ended{} -> Left GameEnded
                                        OnCourse{gamePlayer = p} -> Right p
                
-- | Current board distribution
board :: Monad m => HFiaRT m [[Tile]]
board = get >>= return . gameBoard

-- | If the game ended, returns the result of it
result :: Monad m => HFiaRT m (Either HFiaRError HFiaRResult)
result = get >>= \game -> return $ case game of
                                        OnCourse{} -> Left GameNotEnded
                                        Ended{gameResult = r} -> Right r
