-- | This module defines the HFiaR monad and all the actions you can perform in it
module HFiaR (
    -- MONAD CONTROLS --
    HFiaR, play,
    -- TYPES --
    Player(..), HFiaRError(..), HFiaRResult(..),
    -- ACTIONS --
    restart, dropIn, currentPlayer, board, result
    ) where

import Control.Monad.State

data HFiaRError = GameEnded | GameNotEnded | InvalidColumn | FullColumn
    deriving (Eq)
    
instance Show HFiaRError where
    show GameEnded      = "Game ended"
    show GameNotEnded   = "Game is on curse yet"
    show InvalidColumn  = "That column doesn't exist"
    show FullColumn     = "That column is full"

data Player = Red | Green
    deriving (Eq, Show)
    
data HFiaRResult = Tie | WonBy Player
    deriving (Eq, Show)

data Game = Game {gameEnded  :: Bool,
                  gameResult :: HFiaRResult,
                  gamePlayer :: Player,
                  gameBoard  :: [[Player]]
                 }
    deriving (Eq, Show)
    
newtype HFiaRT m a = HFT {state :: StateT Game m a}
    deriving (Monad, MonadIO, MonadTrans)
    
instance Monad m => MonadState Game (HFiaRT m) where
    get = HFT $ get
    put = HFT . put
    
type HFiaR = HFiaRT IO

play :: Monad m => HFiaRT m a -> m a
play actions = (state actions) `evalStateT` emptyGame

--------------------------------------------------------------------------------
restart :: HFiaR ()
restart = put emptyGame

dropIn :: Int -> HFiaR (Either HFiaRError ())
dropIn c | c < 0 = return $ Left InvalidColumn
         | 6 < c = return $ Left InvalidColumn
         | otherwise =
            do
                game <- get
                if (gameEnded game)
                    then return $ Left GameEnded
                    else if (length $ (gameBoard game) !! c) == 7
                            then return $ Left FullColumn
                            else
                                let player   = gamePlayer game
                                    newBoard = insertAt c player $ gameBoard game
                                    newResult= if (isWinner c player newBoard) then WonBy player else Tie
                                    newEnded = (newResult == WonBy player) || full newBoard
                                 in put game{gameBoard = newBoard,
                                             gamePlayer= otherPlayer $ gamePlayer game,
                                             gameEnded = newEnded,
                                             gameResult= newResult
                                            } >>= return . Right
    where insertAt :: Int -> a -> [[a]] -> [[a]]
          insertAt i x xss = (take i xss) ++ ( (x : (xss !! i)) : drop (i+1) xss)
          
          otherPlayer :: Player -> Player
          otherPlayer Green = Red
          otherPlayer Red = Green
          
          full :: [[a]] -> Bool
          full = all (\x -> 7 == length x)
          
          isWinner :: Int -> Player -> [[Player]] -> Bool
          isWinner c p b =
            let col = b !! c
             in ([p,p,p,p] == take 4 col) ||
                fourIn (getRow (length col - 1) b) ||
                fourIn (getDiagUpRight c (length col - 1) b) ||
                fourIn (getDiagUpLeft  c (length col - 1) b)

          getRow :: Int -> [[Player]] -> [Maybe Player]
          getRow r = map (cell r)
          
          getDiagUpRight :: Int -> Int -> [[Player]] -> [Maybe Player]
          getDiagUpRight c r xss = map (\i -> cell (i+r-c) (xss !! i)) [0..6]
          
          getDiagUpLeft :: Int -> Int -> [[Player]] -> [Maybe Player]
          getDiagUpLeft c r xss = map (\i -> cell (r+c-i) (xss !! i)) [0..6]
          
          cell :: Int -> [Player] -> Maybe Player
          cell c xs = if (c >= 0 && c < length xs)
                        then Just $ (reverse xs) !! c
                        else Nothing

          fourIn :: [Maybe Player] -> Bool
          fourIn [] = False
          fourIn (Nothing:xs) = fourIn xs
          fourIn (Just p :xs) = ([Just p, Just p, Just p] == take 3 xs) || fourIn xs

currentPlayer :: HFiaR (Either HFiaRError Player)
currentPlayer = get >>= \Game{gameEnded = e,
                              gamePlayer= p} ->
                            return $ if e
                                        then Left GameEnded
                                        else Right p

board :: HFiaR [[Player]]
board = get >>= return . gameBoard

result :: HFiaR (Either HFiaRError HFiaRResult)
result = get >>= \Game{gameEnded = e,
                       gameResult= r} ->
                            return $ if (not e)
                                        then Left GameNotEnded
                                        else Right r

--------------------------------------------------------------------------------
emptyGame :: Game
emptyGame = Game{gamePlayer = Green,
                 gameBoard  = replicate 7 [],
                 gameEnded  = False,
                 gameResult = Tie}
