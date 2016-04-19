import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom
import           Data.Monoid
-- import           Control.Monad

main :: IO ()
main = mainWidget atompit

ns = Just "http://www.w3.org/2000/svg"

atompit :: MonadWidget t m => m ()
atompit = do
  elDynAttrNS' ns "svg" (constDyn $ "width" =: "1000" <> "height" =: "1000") $ 
      elStopPropagationNS ns "g" Click $ elDynAttrNS' ns "rect" (constDyn $ "width" =: "100" <> "height" =: "200") $  return ()
  return ()


