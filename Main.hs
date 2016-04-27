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
rowCount=12
colCount=12
dt = 0.03

type Cell = (Int, Int)

data Model = Model { path :: [Cell]
                   , board :: [Cell]
                   }

initModel rc cc = 
    let board = do r <- [0..rc-1] 
                   c <- [0..cc-1] 
                   [(r,c)]
        path = []
    in Model path board

data Action = Tick | SetStart Cell

main :: IO ()
main = do
    startTime <- getCurrentTime
    mainWidget $ do
        rec changes <- view model
            ticks <- fmap (const Tick) <$> tickLossy dt startTime 
            model <- foldDyn update (initModel rowCount colCount) $ mergeWith const [changes, ticks]
        return ()

nextMoves :: Model -> Cell -> [Cell]
nextMoves model@(Model p b) startCell = 
  let c = [ 1,  2, -1, -2]

      km = do cx <- c
              cy <- c
              if abs cx == abs cy then [] else [(cx,cy)]

      jumps = map (\cell -> (fst cell + fst startCell, snd cell + snd startCell)) km

  in filter (\j -> elem j b && notElem j p ) jumps

bestMove :: Model -> Maybe Cell
bestMove model@(Model p _) = 
    let options = (nextMoves model $ head p)
    in if null options 
       then Nothing 
       else Just $ minimumBy (compare `on` (length . nextMoves model)) options

update :: Action -> Model -> Model
update action m@(Model p b) =
    case action of
        SetStart start -> 
            Model [start] b
        Tick ->  
            if null p then m 
            else case bestMove m of
                     Nothing -> m
                     Just best ->  Model (best:p) b
            
ns = Just "http://www.w3.org/2000/svg"

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = do
    let 
        fillColor r c = if (r + c) `mod` 2 == 0 then "blue" else "grey"

        showChecker :: MonadWidget t m => Cell -> m (Event t Action)
        showChecker cell@(r, c) = do
            (el, _) <- elDynAttrNS' ns "rect" 
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

    (el, ev) <- elDynAttrNS' ns "svg" 
                    (constDyn $  "viewBox" =: ("0 0 " ++ show rowCount ++ " " ++ show colCount)
                              <> "width" =: show w
                              <> "height" =: show h)
                $ elStopPropagationNS ns "g" Click $ render model
    return ev
