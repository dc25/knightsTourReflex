{-# LANGUAGE RecursiveDo #-}
import           Data.Map (Map)
import           Data.List (minimumBy)
import qualified Data.Map as M
import           Reflex.Dom
import           Reflex.Dom.Time
import           Data.Time.Clock
import           Data.Monoid ((<>))
import           Data.Function (on)

w = 450
h = 450
rowCount=14
colCount=14
dt = 0.05

type Cell = (Int, Int)

data Model = Model 
    { path :: [Cell]
    , board :: [Cell]
    }

data Action = Tick | SetStart Cell

initModel = 
    let board = do r <- [0..rowCount-1] 
                   c <- [0..colCount-1] 
                   [(r,c)]
        path = []
    in Model path board

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = do
    let 
        ns = Just "http://www.w3.org/2000/svg"

        fillColor r c = if (r + c) `mod` 2 == 0 then "blue" else "grey"

        showChecker :: MonadWidget t m => Cell -> m (Event t Action)
        showChecker cell@(r, c) = do
            (el, ev) <- elDynAttrNS' ns "rect" 
                           (constDyn $  "x" =: show c 
                                     <> "y" =: show r 
                                     <> "width" =: "1" 
                                     <> "height" =: "1" 
                                     <> "fill" =: fillColor r c)
                       $ return ()
            return $ const (SetStart cell) <$> domEvent Click el 

        showMove :: MonadWidget t m => (Cell, Cell) -> m (Event t Action)
        showMove (pt0, pt1) = do
            elDynAttrNS' ns "line" 
                 (constDyn $  "x1" =: show ((snd pt0 & fromIntegral :: Float) + 0.5)
                           <> "y1" =: show ((fst pt0 & fromIntegral :: Float) + 0.5)
                           <> "x2" =: show ((snd pt1 & fromIntegral :: Float) + 0.5)
                           <> "y2" =: show ((fst pt1 & fromIntegral :: Float) + 0.5)
                           <> "style" =: "stroke:yellow;stroke-width:0.05" ) 
                 $ return ()
            return never

        render :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
        render model = do
            checkerMap <- mapDyn (M.fromList . map (\c -> (c,())) . board) model
            checkerEvs <- listWithKey checkerMap (\c _ -> showChecker c)
            checkerEv <- mapDyn (leftmost . M.elems) checkerEvs

            let getMoves model@(Model path board) = zip path $ tail path
            moveMap <- mapDyn (M.fromList . map (\c -> (c,())) . getMoves) model
            listWithKey moveMap (\c _ -> showMove c)

            return $ switchPromptlyDyn checkerEv

    unvisited <- mapDyn (\model -> "Unvisited count : " ++ show ((length.board) model - (length.path) model)) model

    el "div" $ do
        el "h2" $ text "Knight's Tour"
        el "h2" $ dynText unvisited
        el "h2" $ text "(pick a square)"
        (_, ev) <- elDynAttrNS' ns "svg" 
                        (constDyn $  "viewBox" =: ("0 0 " ++ show rowCount ++ " " ++ show colCount)
                                  <> "width" =: show w
                                  <> "height" =: show h)
                    $ elStopPropagationNS ns "g" Click $ render model
        return ev

nextMoves :: Model -> Cell -> [Cell]
nextMoves model@(Model path board) startCell = 
  let c = [ 1,  2, -1, -2]

      km = do cx <- c
              cy <- c
              if abs cx == abs cy then [] else [(cx,cy)]

      jumps = map (\cell -> (fst cell + fst startCell, snd cell + snd startCell)) km

  in filter (\j -> elem j board && notElem j path ) jumps

bestMove :: Model -> Maybe Cell
bestMove model@(Model path _) = 
    let options = (nextMoves model $ head path)
    in if null options 
       then Nothing 
       else Just $ minimumBy (compare `on` (length . nextMoves model)) options

update :: Action -> Model -> Model
update action model@(Model path board) =
    case action of
        SetStart start -> 
            Model [start] board
        Tick ->  
            if null path then model 
            else case bestMove model of
                     Nothing -> model
                     Just best ->  Model (best:path) board

main :: IO ()
main = do
    startTime <- getCurrentTime
    mainWidget $ do
        rec selection <- view model
            ticks <- fmap (const Tick) <$> tickLossy dt startTime 
            model <- foldDyn update initModel $ mergeWith const [selection, ticks]
        return ()

