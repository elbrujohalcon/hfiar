module HFiaR.AI (aiDropIn) where

import HFiaR
import Data.Maybe

-- | Drop a tile in a column choosen by the Artificial Inteligence
aiDropIn :: Monad m => HFiaRT m (Either HFiaRError ())
aiDropIn =
    do
        r <- result;
        case r of
            Left GameNotEnded ->
                bestColumn >>= dropIn 
            Left err ->
                return $ Left err
            Right _ ->
                return $ Left GameEnded

bestColumn :: Monad m => HFiaRT m Int
bestColumn =
    do
        j1 <- columnWhereWins
        j2 <- columnWhereLoses
        j3 <- column3IfAvailable
        j4 <- firstAvailableColumn
        return . head $ catMaybes [j1, j2, j3, j4]

columnWhereWins, columnWhereLoses,
    column3IfAvailable, firstAvailableColumn :: Monad m => HFiaRT m (Maybe Int)

columnWhereWins = mapM (tryDropIn . (:[])) [0..6] >>= return . firstEnded

columnWhereLoses = mapM moves [0..6] >>= return . firstEnded
    where moves :: Monad m => Int -> HFiaRT m (Either HFiaRError Game)
          moves col = do
            b <- board
            let avail c = c == col || length (b!!c) == 7
            other <- return . length $ takeWhile avail [0..6]
            tryDropIn [other, col]

column3IfAvailable = board >>= \b -> return $ case length (b !! 3) of
                                                    7 -> Nothing
                                                    _ -> Just 3

firstAvailableColumn = board >>=
                        return . Just . length . takeWhile (\c -> length c == 7)

firstEnded :: [Either HFiaRError Game] -> Maybe Int
firstEnded games = case (length $ takeWhile onCourse games) of
                        7 -> Nothing
                        g -> Just g
    where onCourse (Left _)           = False
          onCourse (Right OnCourse{}) = True
          onCourse (Right Ended{})    = False 