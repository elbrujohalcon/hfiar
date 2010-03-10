-- | This module provides the HFiaR GUI
module HFiaR.GUI ( gui ) where

-- import qualified HFiaR.Server as HFS
import Data.List (transpose)
import Control.Monad
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WX

data GUIColumn  = GUICol { colNumber    :: Int,
                           colCells     :: [StaticText ()], --TODO: Use images
                           colButton    :: Button () }

data GUIContext = GUICtx { guiWin       :: Frame (),
                           guiPlayer    :: StaticText (),
                           guiColumns   :: [GUIColumn] }

gui :: IO ()
gui = do
        -- model <- HFS.start
        
        win <- frame [text := "Four in a Row"]
        
        -- set win [on closing := HFS.stop model >> propagateEvent]
        
        player <- staticText win [text := "Green Player Turn"]
        
        columns <- forM [1..7] (\c ->
                                do
                                  cells <- forM [1..7] (\r -> staticTextCreate win (cellId c r) "O" rectNull 0)
                                  btn   <- button win [identity     := buttonId c,
                                                       text         := "Select"]
                                  return $ GUICol c cells btn
                                  )
        
        status <- statusField [text := ""] --NOTE: Just decorative
        set win [statusBar := [status]]
        
        let guiCtx = GUICtx win player columns 
        -- let onCmd acc = acc model guiCtx
        let onCmd acc = acc guiCtx
        
        forM_ columns $ \GUICol{colButton = b, colNumber = c} -> set b [on command := onCmd $ selectColumn c]
        
        -- Menu bar...
        mnuGame <- menuPane [text := "Game"]
        menuAppend mnuGame 5002 "&New\tCtrl-n" "New Game" False
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

selectColumn :: Int -> GUIContext -> IO ()
selectColumn c GUICtx{guiWin = win} = infoDialog win "Four in a Row" $ "Column " ++ (show c) ++ " selected" 

cellId :: Int -> Int -> Id
cellId c r = 5300 + c * 10 + r

buttonId :: Int -> Id
buttonId c = 5300 + c * 10