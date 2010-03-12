-- | This module defines the HFiaR monad and all the actions you can perform in it
module HFiaR (
    -- MONAD CONTROLS --
    HFiaR, play,
    -- TYPES --
    Player(..), HFiaRError(..)
    ) where

import Control.Monad.State

data HFiaRError = GameEnded | OtherError String
    deriving (Eq)
    
instance Show HFiaRError where
    show GameEnded      = "Game ended"
    show (OtherError s) = s

data Player = Red | Green | None
    deriving (Eq, Show)

data Game = Game {
                  player :: Player,
                  board  :: [[Player]]
                 }
    deriving (Eq, Show)
    
newtype HFiaRT m a = HFT {state :: StateT Game m a}
    deriving (Monad, MonadIO, MonadTrans)
    
instance Monad m => MonadState Game (HFiaRT m) where
    get = HFT $ get
    put = HFT . put
    
type HFiaR = HFiaRT IO

play :: HFiaR a -> IO a
play actions = (state actions) `evalStateT` Game{player  = Green,
                                                 board   = replicate 7 []}