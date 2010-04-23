module HFiaR.AI (aiDropIn) where

import HFiaR

-- | Drop a tile in a column choosen by the Artificial Inteligence
aiDropIn :: Monad m => HFiaRT m (Either HFiaRError ())
aiDropIn =
	do {
		r <- result;
		case r of {
			Left GameNotEnded ->
				do {
					Right p <- player;
					b <- board;
					dropIn $ bestColumnFor p b
					};
			Left err ->
				return $ Left err;
			Right _ ->
				return $ Left GameEnded
		}
	}

bestColumnFor :: Player -> [[Tile]] -> Int
bestColumnFor p b =
	case columnWhereWins p b of
		Nothing ->
			case columnWhereLoses p b of
				Nothing ->
					case length (b !! 3) of
						7 -> length $ takeWhile (\c -> length c == 7) b
						_ -> 3
				Just cwl -> cwl
		Just cww -> cww

columnWhereWins :: Player -> [[Tile]] -> Maybe Int
columnWhereWins Pl{tiles=tile} b =
	case (length $ takeWhile (not . wins tile b) [0..6]) of
		7 -> Nothing
		x -> Just x

columnWhereLoses :: Player -> [[Tile]] -> Maybe Int
columnWhereLoses Pl{tiles=tile} b =
	let otherTile = case tile of {Green -> Red; Red -> Green}
	 in case (length $ takeWhile (not . wins otherTile b) [0..6]) of
			7 -> Nothing
			x -> Just x

wins :: Tile -> [[Tile]] -> Int -> Bool
wins t b c =
	let	newBoard = (take c b) ++ ((t : (b !! c)) : drop (c+1) b)
		col = newBoard !! c
		getRow r = map (cell r)
		getDiagUpRight c r xss = map (\i -> cell (i+r-c) (xss !! i)) [0..6]
		getDiagUpLeft c r xss = map (\i -> cell (r+c-i) (xss !! i)) [0..6]
		cell c xs = if (c >= 0 && c < length xs)
					then Just $ (reverse xs) !! c
					else Nothing
		fourIn [] = False
		fourIn (Nothing:xs) = fourIn xs
		fourIn (Just p :xs) = ([Just p, Just p, Just p] == take 3 xs) || fourIn xs
      in	([t,t,t,t] == take 4 col) ||
      	fourIn (getRow (length col - 1) newBoard) ||
		fourIn (getDiagUpRight c (length col - 1) newBoard) ||
		fourIn (getDiagUpLeft  c (length col - 1) newBoard)

testGame :: [[Tile]]
testGame = justEval $ aiDropIn >> aiDropIn >>
				  aiDropIn >> aiDropIn >>
				  aiDropIn >> aiDropIn >>
				  aiDropIn >> aiDropIn >> board