import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom

main :: IO ()
main = mainWidget atompit

atompit :: MonadWidget t m => m ()
atompit = do
  let ns = Just "http://www.w3.org/2000/svg"

  let svgAttrs :: Map String String
      svgAttrs = M.fromList [ ("version", "1.1")
                 , ("baseProfile", "full")
                 , ("width", "300px")
                 , ("height", "300px")
                 , ("viewBox", "0 0 8 8") ]

      showChecker :: MonadWidget t m => Int -> Int -> m ()
      showChecker row col = 
          elWith "rect" (ElConfig ns (M.fromList [("x", show row), ("y", show col), ("width", "1"), ("height", "1"), ("fill", if (row + col) `mod` 2 == 0 then "blue" else "grey")])) $ return ()

      checkers = [0..7] >>= \r -> 
                 [0..7] >>= \c -> 
                 [showChecker r c]

  elWith "svg" (ElConfig ns svgAttrs) $ 
    sequence_ checkers
