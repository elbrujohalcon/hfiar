module HFiaR.AI (aiDropIn) where

import HFiaR

-- | Drop a tile in a column choosen by the Artificial Inteligence
aiDropIn :: Monad m => HFiaRT m (Either HFiaRError ())
aiDropIn =
	do
		r <- result;
		case r of
			Left GameNotEnded ->
				do
					Right p <- player;
					b <- board;
					dropIn $ bestColumnFor p b
			Left err ->
				return $ Left err
			Right _ ->
				return $ Left GameEnded

bestColumnFor :: Player -> [[Tile]] -> Int
bestColumnFor _p = length . takeWhile (\c -> length c == 7)

testGame :: IO [[Tile]]
testGame = justEval $ aiDropIn >> aiDropIn >>
				  aiDropIn >> aiDropIn >>
				  aiDropIn >> aiDropIn >>
				  aiDropIn >> aiDropIn >>
				  board