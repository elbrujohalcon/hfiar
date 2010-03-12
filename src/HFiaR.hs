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
                            else put game{gameBoard = insertAt c (gamePlayer game) $ gameBoard game,
                                          gamePlayer= otherPlayer $ gamePlayer game
                                          --TODO: Verify if the player won (or maybe it's a tie) and change the state acordingly
                                         } >>= return . Right
    where insertAt :: Int -> a -> [[a]] -> [[a]]
          insertAt i x xss = (take i xss) ++ ( (x : (xss !! i)) : drop (i+1) xss)
          otherPlayer :: Player -> Player
          otherPlayer Green = Red
          otherPlayer Red = Green

currentPlayer :: HFiaR (Either HFiaRError Player)
currentPlayer = get >>= \Game{gameEnded = e,
                              gamePlayer= p} ->
                            return $ if e
                                        then Left GameEnded
                                        else Right p

board :: HFiaR (Either HFiaRError [[Player]])
board = get >>= \Game{gameEnded = e,
                      gameBoard = b} ->
                            return $ if e
                                        then Left GameEnded
                                        else Right b

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
