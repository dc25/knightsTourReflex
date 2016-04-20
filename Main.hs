{-# LANGUAGE RecursiveDo #-}

import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom
import           Data.Monoid ((<>))

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

data Action = Tick Int | SetStart Cell

main :: IO ()
main = mainWidget $ do
    rec changes <- view model
        model <- foldDyn update (initModel 8 8) changes
    return ()

update :: Action -> Model -> Model
update action m@(Model r c p b) =
    case action of
        SetStart start -> Model r c [start] b
        Tick t ->  Model r c p b
            
ns = Just "http://www.w3.org/2000/svg"

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
-- view :: MonadWidget t m => m ()
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
            let ckrs = [0..7] >>= \r -> 
                       [0..7] >>= \c -> 
                       [showChecker r c]
            in sequence ckrs -- tricky
    
    (el, ev) <- elDynAttrNS' ns "svg" 
                    (constDyn $  "viewBox" =: "0 0 8 8" 
                              <> "width" =: "500"
                              <> "height" =: "500") 
                $ elStopPropagationNS ns "g" Click (fmap leftmost showCheckers)
    return ev

