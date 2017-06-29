module ImageRecognition where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           System.Environment

isMiddlePixelRed :: FilePath -> IO (Maybe Bool)
isMiddlePixelRed fp = do
    image <- readImage fp
    case image of
        Left _       -> return Nothing
        Right image' -> return (go image')
  where
    go :: DynamicImage -> Maybe Bool
    go (ImageRGB8 image@(Image w h _)) =
        Just (isRed (pixelAt image (w `div` 2) (h `div` 2)))
    go _ = Nothing
    isRed :: PixelRGB8 -> Bool
    isRed (PixelRGB8 r g b) = r == maxBound && g == 0 && b == 0
