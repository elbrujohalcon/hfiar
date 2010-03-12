-- | This module provides the HFiaR GUI
module HFiaR.GUI ( gui ) where

import HFiaR
import qualified HFiaR.Server as HFS
import Data.List (transpose)
import Control.Monad
import Graphics.UI.WXCore
import Graphics.UI.WX

data GUIColumn  = GUICol { colNumber    :: Int,
                           colCells     :: [StaticText ()], --TODO: Use images
                           colButton    :: Button () }

data GUIContext = GUICtx { guiWin       :: Frame (),
                           guiPlayer    :: StaticText (),
                           guiColumns   :: [GUIColumn] }

gui :: IO ()
gui = do
        model <- HFS.start
        
        win <- frame [text := "Four in a Row"]
        
        set win [on closing := HFS.stop model >> propagateEvent]
        
        player <- staticText win [text := "Green Player Turn"]
        
        columns <- forM [0..6] (\c ->
                                do
                                  cells <- forM [0.. 6] (\r -> staticTextCreate win (cellId c r) "" rectNull 0)
                                  btn   <- button win [identity     := buttonId c,
                                                       text         := "Select"]
                                  return $ GUICol c cells btn
                                  )
        
        status <- statusField [text := ""] --NOTE: Just decorative
        set win [statusBar := [status]]
        
        let guiCtx = GUICtx win player columns 
        
        forM_ columns $ \GUICol{colButton = b, colNumber = c} ->
                            set b [on command := do
                                                    selectColumn c guiCtx model
                                                    refreshGUI guiCtx model]
        
        -- Menu bar...
        mnuGame <- menuPane [text := "Game"]
        menuAppend mnuGame 5002 "&New\tCtrl-n" "New Game" False
        evtHandlerOnMenuCommand win 5002 $ restartGame guiCtx model >> refreshGUI guiCtx model
        menuQuit mnuGame [on command := wxcAppExit]
        mnuHelp <- menuHelp []
        menuAppend mnuHelp 5009 "&Instructions\tCtrl-h" "Open the Instructions Page" False
        menuAbout mnuHelp [on command := infoDialog win "About HFiaR" "Author: Fernando Brujo Benavides"]
        set win [menuBar := [mnuGame, mnuHelp]]
        
        let columnLayout GUICol{colCells = cs, colButton = b} =
                (hfill $ widget b) : (map (fill . boxed "" . floatCenter . widget) cs)
        set win [layout := column 5 [hfill . boxed "" . floatCenter $ widget player,
                                     grid 1 1 . transpose $ map columnLayout columns],
                 clientSize := sz 500 500]


-------------------------------------------------------------------------------------------------------------------------
selectColumn :: Int -> GUIContext -> HFS.ServerHandle -> IO ()
selectColumn c GUICtx{guiWin = win} model =
    do
        res <- HFS.runIn model $ dropIn c
        case res of
            Left err ->
                errorDialog win "Four in a Row" $ show err
            Right () ->
                return ()

restartGame :: GUIContext -> HFS.ServerHandle -> IO ()
restartGame _guiCtx model = HFS.runIn model $ restart --TODO: Verify if the player wants to restart the game even if it hasn't ended yet

refreshGUI :: GUIContext -> HFS.ServerHandle -> IO ()
refreshGUI GUICtx{guiPlayer = player, guiColumns = columns, guiWin = win} model =
    do
        res1 <- HFS.runIn model currentPlayer
        case res1 of
            Left GameEnded ->
                do
                    forM_ columns $ \GUICol{colButton = b} -> set b [enabled := False]
                    res2 <- HFS.runIn model result
                    case res2 of
                        Left err ->
                            errorDialog win "Four in a Row" $ show err
                        Right Tie ->
                            set player [text := "It was a tie"]
                        Right (WonBy Green) ->
                            set player [text := "Green Player won"]
                        Right (WonBy Red) ->
                            set player [text := "Red Player won"]
            Left err ->
                errorDialog win "Four in a Row" $ show err
            Right p ->
                do
                    set player [text := (show p) ++ " Player turn"]
                    res2 <- HFS.runIn model board
                    case res2 of
                        Left err ->
                            errorDialog win "Four in a Row" $ show err
                        Right cols ->
                            forM_ columns $ \GUICol{colCells = cells,
                                                    colNumber= coln} ->
                                                do
                                                    forM_ cells $ \cell -> set cell [text := ""]
                                                    zipWithM_ (\cell val -> set cell [text := show val])
                                                              cells (cols !! coln)

-------------------------------------------------------------------------------------------------------------------------
cellId :: Int -> Int -> Id
cellId c r = 5300 + c * 10 + r

buttonId :: Int -> Id
buttonId c = 5300 + c * 10