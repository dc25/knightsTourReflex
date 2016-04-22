{-# LANGUAGE RecursiveDo #-}
import           Data.Map (Map)
import           Data.List (minimumBy)
import qualified Data.Map as M
import           Reflex.Dom
import           Reflex.Dom.Time
import           Data.Time.Clock
import           Data.Monoid ((<>))
import           Data.Function (on)

type Cell = (Int, Int)

data Model = Model { rows :: Int
                   , cols :: Int
                   , path :: [Cell]
                   , board :: [Cell]
                   }

initModel rc cc = 
    let board = do r <- [0..rc-1] 
                   c <- [0..cc-1] 
                   [(r,c)]
        path = []
    in Model rc cc path board

data Action = Tick | SetStart Cell

main :: IO ()
main = do
    startTime <- getCurrentTime
    mainWidget $ do
        rec changes <- view model
            ticks <- fmap (const Tick) <$> tickLossy 1 startTime 
            model <- foldDyn update (initModel 8 8) $ mergeWith const [changes, ticks]
        return ()

nextMoves :: Model -> Cell -> [Cell]
nextMoves model@(Model r c p b) startCell = 
  let c = [ 1,  2, -1, -2]

      km = do cx <- c
              cy <- c
              if abs cx == abs cy then [] else [(cx,cy)]

      jumps = map (\cell -> (fst cell + fst startCell, snd cell + snd startCell)) km

  in filter (\j -> elem j b && notElem j p ) jumps

bestMove :: Model -> Maybe Cell
bestMove model@(Model _ _ p _) = 
    let options = (nextMoves model $ head p)
    in if null options 
       then Nothing 
       else Just $ minimumBy (compare `on` (length . nextMoves model)) options

update :: Action -> Model -> Model
update action m@(Model r c p b) =
    case action of
        SetStart start -> 
            Model r c [start] b
        Tick ->  
            if null p then m 
            else case bestMove m of
                     Nothing -> m
                     Just best ->  Model r c (best:p) b
            
ns = Just "http://www.w3.org/2000/svg"

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = do
    let 
        fillColor r c = if (r + c) `mod` 2 == 0 then "blue" else "grey"

        showChecker :: MonadWidget t m => Int -> Int -> m (Event t Action)
        showChecker r c = do
            (el, _) <- elDynAttrNS' ns "rect" 
                           (constDyn $  "x" =: show c 
                                     <> "y" =: show r 
                                     <> "width" =: "1" 
                                     <> "height" =: "1" 
                                     <> "fill" =: fillColor r c)
                       $ return ()
            return $ const (SetStart (r,c)) <$> domEvent Click el -- tricky

        checkers r c = 
            let ckrs = do row <- [0..r-1] 
                          col <- [0..c-1] 
                          [showChecker row col]
            in sequence ckrs -- tricky

        showMove :: MonadWidget t m => Cell -> Cell -> m (Event t Action)
        showMove pt0 pt1 = do
            elDynAttrNS' ns "line" 
                 (constDyn $  "x1" =: show ((snd pt0 & fromIntegral :: Float) + 0.5)
                           <> "y1" =: show ((fst pt0 & fromIntegral :: Float) + 0.5)
                           <> "x2" =: show ((snd pt1 & fromIntegral :: Float) + 0.5)
                           <> "y2" =: show ((fst pt1 & fromIntegral :: Float) + 0.5)
                           <> "style" =: "stroke:yellow;stroke-width:0.05" ) 
                 $ return ()
            return never

        moves (Model _ _ p _) = 
            if null p then [] else zipWith showMove p $ tail p
    
    (el, ev) <- elDynAttrNS' ns "svg" 
                    (constDyn $  "viewBox" =: "0 0 8 8" 
                              <> "width" =: "500"
                              <> "height" =: "500") 
                $ elStopPropagationNS ns "g" Click (leftmost <$> checkers 8 8)
    return ev
