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
                   return (r,c)
    in Model rc cc [] board

data Action = Tick | SetStart Cell

main :: IO ()
main = do
    startTime <- getCurrentTime
    mainWidget $ do
        rec changes <- view model
            ticks <- fmap (const Tick) <$> tickLossy 1 startTime 
            let actions = mergeWith const [changes, ticks]
            model <- foldDyn update (initModel 8 8) actions
        return ()

nextMoves :: Model -> Cell -> [Cell]
nextMoves model@(Model r c p b) startCell = 
  let c = [ 1,  2, -1, -2]

      km = do cx <- c
              cy <- c
              if abs cx == abs cy then [] else [(cx,cy)]

      jumps = map (\cell -> (fst cell + fst startCell, snd cell + snd startCell)) km
  in filter (\j -> elem j b && notElem j p ) jumps

bestMove :: Model -> Cell
bestMove model@(Model _ _ p _) = 
    minimumBy (compare `on` (length . nextMoves model)) (nextMoves model $ head p)

update :: Action -> Model -> Model
update action m@(Model r c p b) =
    case action of
        SetStart start -> 
            Model r c [start] b
        Tick ->  
            if null p then 
                m
            else
                Model r c (bestMove m:p) b
            
ns = Just "http://www.w3.org/2000/svg"

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view m = do
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

        showCheckers = 
            let ckrs = do r <- [0..7] 
                          c <- [0..7] 
                          return $ showChecker r c
            in sequence ckrs -- tricky
    
    (el, ev) <- elDynAttrNS' ns "svg" 
                    (constDyn $  "viewBox" =: "0 0 8 8" 
                              <> "width" =: "500"
                              <> "height" =: "500") 
                $ elStopPropagationNS ns "g" Click (fmap leftmost showCheckers)
    return ev

