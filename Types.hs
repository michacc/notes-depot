module Types where

{- Module with definitions of common types used in the application. -}

import Prelude
import Data.Char(isDigit)
import qualified Data.Text as T
import Network.HTTP.Base(urlEncode, urlDecode)
import Web.PathPieces(PathPiece(..))

newtype PageNumber = PageNumber Int
    deriving (Show, Eq, Read)

instance PathPiece PageNumber where
    fromPathPiece text
        | T.all isDigit text =
            let number = read . T.unpack $ text
            in
                if number > 0
                    then Just . PageNumber $ number
                    else Nothing
        | otherwise = Nothing
    toPathPiece (PageNumber number)
        | number > 0 = T.pack . show $ number
        | otherwise = "1"

newtype URLEncodedText = URLEncodedText T.Text
    deriving (Show, Eq, Read)

instance PathPiece URLEncodedText where
    fromPathPiece = Just . URLEncodedText . T.pack . urlDecode . T.unpack
    toPathPiece (URLEncodedText text) = T.pack . urlEncode . T.unpack $ text
