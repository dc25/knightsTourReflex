{-# LANGUAGE RecursiveDo #-}
import           Data.Map (Map, fromList, elems)
import           Data.List (minimumBy)
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

type Model = [Cell]

board = do r <- [0..rowCount-1] 
           c <- [0..colCount-1] 
           [(r,c)]

data Action = Tick | SetStart Cell

initModel = []

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

        showMove :: MonadWidget t m => (Cell, Cell) -> m (Event t ())
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
            checkerEv <- fmap leftmost $ sequence $ fmap showChecker board

            let getMoves model = zip model $ tail model
            moveMap <- mapDyn (fromList . map (\c -> (c,())) . getMoves) model
            listWithKey moveMap (\c _ -> showMove c)

            return checkerEv

        center = "style" =: "text-align: center;"

    unvisited <- mapDyn (\m -> "unvisited count = " ++ show (length board - length m)) model

    el "div" $ do
        elAttr "h2" center $ text "Knight's Tour"
        elAttr "h2" center $ dynText unvisited
        elAttr "h2" center $ text "(pick a square)"
        elAttr "div" center $ do
            (_, ev) <- elDynAttrNS' ns "svg" 
                            (constDyn $  "viewBox" =: ("0 0 " ++ show rowCount ++ " " ++ show colCount)
                                      <> "width" =: show w
                                      <> "height" =: show h)
                        $ elStopPropagationNS ns "g" Click $ render model
            return ev

nextMoves :: Model -> Cell -> [Cell]
nextMoves model startCell = 
  let c = [ 1,  2, -1, -2]

      km = do cx <- c
              cy <- c
              if abs cx == abs cy then [] else [(cx,cy)]

      jumps = map (\cell -> (fst cell + fst startCell, snd cell + snd startCell)) km

  in filter (\j -> elem j board && notElem j model ) jumps

bestMove :: Model -> Maybe Cell
bestMove model = 
    let options = (nextMoves model $ head model)
    in if null options 
       then Nothing 
       else Just $ minimumBy (compare `on` (length . nextMoves model)) options

update :: Action -> Model -> Model
update action model =
    case action of
        SetStart start -> [start] 
        Tick ->  
            if null model then model 
            else case bestMove model of
                     Nothing -> model
                     Just best -> best:model

main :: IO ()
main = do
    startTime <- getCurrentTime
    mainWidget $ do
        ticks <- fmap (const Tick) <$> tickLossy dt startTime 
        rec selection <- view model
            model <- foldDyn update initModel $ mergeWith const [selection, ticks]
        return ()

