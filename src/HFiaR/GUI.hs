-- | This module provides the HFiaR GUI
module HFiaR.GUI ( gui ) where

import HFiaR
import HFiaR.AI
import qualified HFiaR.Server as HFS
import Data.List (transpose)
import Control.Monad
import Graphics.UI.WXCore
import Graphics.UI.WX

data GUIColumn  = GUICol { colNumber    :: Int,
                           colCells     :: [Panel ()],
                           colButton    :: Button () }

data GUIContext = GUICtx { guiPlayers   :: Var Int, 
                           guiWin       :: Frame (),
                           guiPlayer    :: StaticText (),
                           guiColumns   :: [GUIColumn],
                           guiModel     :: Var HFS.ServerHandle }

gui :: IO ()
gui = do
        model <- HFS.start
        modelVar <- varCreate model
        
        win <- frame [text := "Four in a Row"]
        
        set win [on closing := HFS.stop model >> propagateEvent]
        
        playerText <- staticText win [text := "Green Player Turn"]
        
        theBoard <- forM [0..6] $ \c ->
                                    do
                                      cells <- forM [0.. 6] (\r -> panelCreate win (cellId c r) rectNull 0)
                                      forM_ cells $ \cell -> set cell [bgcolor := grey]
                                      btn   <- button win [identity     := buttonId c,
                                                           bgcolor      := grey,
                                                           text         := "Select"]
                                      return $ GUICol c cells btn
        
        status <- statusField [text := ""] --NOTE: Just decorative
        set win [statusBar := [status]]
        
        playersVar <- varCreate 2
        
        let guiCtx = GUICtx playersVar win playerText theBoard modelVar
        
        forM_ theBoard $ \GUICol{colButton = b, colNumber = c} ->
                            set b [on command := do
                                                    selectColumn c guiCtx
                                                    refreshGUI guiCtx]
        
        -- Menu bar...
        mnuGame <- menuPane [text := "Game"]
        menuAppend mnuGame 5002 "&New (2 Players)\tCtrl-n" "New Game (2 Players)" False
        menuAppend mnuGame 5003 "&New (1 Player)\tCtrl-Shift-n" "New Game (1 Player)" False
        evtHandlerOnMenuCommand win 5002 $ varSet playersVar 2 >> restartGame guiCtx >> refreshGUI guiCtx
        evtHandlerOnMenuCommand win 5003 $ varSet playersVar 1 >> restartGame guiCtx >> refreshGUI guiCtx
        _quit <- menuQuit mnuGame [on command := wxcAppExit]
        mnuHelp <- menuHelp []
        menuAppend mnuHelp 5009 "&Instructions\tCtrl-h" "Open the Instructions Page" False
        _about <- menuAbout mnuHelp [on command := infoDialog win "About HFiaR" "Author: Fernando Brujo Benavides"]
        set win [menuBar := [mnuGame, mnuHelp]]
        
        let columnLayout GUICol{colCells = cs, colButton = b} =
                (hfill $ widget b) : (map (fill . widget) $ reverse cs)
        set win [layout := column 5 [hfill . boxed "" . floatCenter $ widget playerText,
                                     grid 1 1 . transpose $ map columnLayout theBoard],
                 clientSize := sz 500 500]


-------------------------------------------------------------------------------------------------------------------------
selectColumn :: Int -> GUIContext -> IO ()
selectColumn c GUICtx{guiPlayers = playersVar, guiWin = win, guiModel = modelVar} =
    do
        model <- varGet modelVar
        players <- varGet playersVar
        res <- HFS.runIn model $ dropIn c
        case res of
            Left err ->
                errorDialog win "Four in a Row" $ show err
            Right () ->
                case players of
                    1 -> do
                            res2 <- HFS.runIn model $ aiDropIn
                            case res2 of
                                Left err ->
                                    errorDialog win "Four in a Row" $ show err
                                Right () ->
                                    return ()
                    _ -> return ()

restartGame :: GUIContext -> IO ()
restartGame GUICtx{guiModel = modelVar} =
    --TODO: Verify if the player wants to restart the game even if it hasn't ended yet
    do
        model <- varGet modelVar
        HFS.stop model
        newModel <- HFS.start
        varSet modelVar newModel

refreshGUI :: GUIContext -> IO ()
refreshGUI GUICtx{guiPlayer = playerText, guiColumns = theBoard, guiWin = win, guiModel = modelVar} =
    do
        model <- varGet modelVar
        res1 <- HFS.runIn model player
        case res1 of
            Left GameEnded ->
                do
                    forM_ theBoard $ \GUICol{colButton = b} -> set b [enabled := False]
                    res2 <- HFS.runIn model result
                    case res2 of
                        Left err ->
                            errorDialog win "Four in a Row" $ show err
                        Right Tie ->
                            set playerText [text := "It was a tie"]
                        Right (WonBy p) ->
                            set playerText [text := show p ++ " Player won!!"]
            Left err ->
                errorDialog win "Four in a Row" $ show err
            Right p ->
                do
                    forM_ theBoard $ \GUICol{colButton = b} -> set b [enabled := True]
                    set playerText [text := (show p) ++ " Player turn"]
        cols <- HFS.runIn model board
        forM_ theBoard $ \GUICol{colCells = cells,
                                 colNumber= coln} ->
                            do
                                forM_ cells $ \cell -> set cell [bgcolor := grey]
                                zipWithM_ (\cell val -> set cell [bgcolor := case val of
                                                                                Red   -> red
                                                                                Green -> green
                                                                 ])
                                            cells (reverse $ cols !! coln)

-------------------------------------------------------------------------------------------------------------------------
cellId :: Int -> Int -> Id
cellId c r = 5300 + c * 10 + r

buttonId :: Int -> Id
buttonId c = 5300 + c * 10