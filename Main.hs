{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import           Data.FileEmbed
import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom

main :: IO ()
-- main = mainWidgetWithCss $(embedFile "style.css") atompit
main = mainWidget atompit

atompit :: MonadWidget t m => m ()
atompit = do
  let ns = Just "http://www.w3.org/2000/svg"
  let svgAttrs :: Map String String
      svgAttrs = [ ("version", "1.1")
                 , ("baseProfile", "full")
                 , ("width", "300px")
                 , ("height", "200px")
                 , ("viewBox", "0 0 300 200") ]
  elWith "svg" (ElConfig ns svgAttrs) $ 
    elWith "rect" (ElConfig ns (M.fromList [("width", "100%"), ("height", "100%"), ("fill", "red")])) (return ())
