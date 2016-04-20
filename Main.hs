import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom
import           Data.Monoid

main :: IO ()
main = mainWidget atompit

ns = Just "http://www.w3.org/2000/svg"

atompit :: MonadWidget t m => m ()
atompit = do
    let 
        showChecker :: MonadWidget t m => Int -> Int -> m (Event t ())
        showChecker r c = do
            (el, ev) <- elDynAttrNS' ns "rect" (constDyn $ "x" =: show c <> "y" =: show r <> "width" =: "1" <> "height" =: "1" <> "fill" =: if (r + c) `mod` 2 == 0 then "blue" else "grey") $ return ()
            return $ domEvent Click el

        showCheckers :: MonadWidget t m => m [Event t ()]
        showCheckers = 
            let ckrs = [0..7] >>= \r -> 
                       [0..7] >>= \c -> 
                       [showChecker r c]
            in sequence ckrs -- tricky
    
    
    elDynAttrNS' ns "svg" (constDyn $ "viewBox" =: "0 0 8 8" <>  "width" =: "500" <> "height" =: "500") $ do
        ckrs <- showCheckers
        let clickEvent = return (leftmost ckrs) -- tricky
        elStopPropagationNS ns "g" Click clickEvent
    return ()

