{-# LANGUAGE OverloadedStrings #-}

module AesonLens where


import           Control.Lens
import           Control.Lens.Aeson
import           Data.Aeson
import qualified Data.ByteString    as B
import           Data.Text          (Text)


data Color = Color { colorName :: !Text }


instance FromJSON Color where
    parseJSON (Object v) = Color <$> (v .: "color")


main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  case eitherDecodeStrict' bs of
    Left e       -> error e
    Right colors -> print $ map colorName colors

-- Using Lens
