module Splay.Util
    ( filePath
    , PixelRGB8(..)
) where

import Codec.Picture
import Data.Binary
import Text.Printf


-- Get a splay file path based on a directory
-- and x,y coordinate
filePath :: FilePath -> Int -> Int -> FilePath
filePath = printf "%s/x%09dy%09d.splay"

-- Instance of Binary to allow serialization of
-- Pixel data to file.
instance Binary PixelRGB8 where
    put (PixelRGB8 r g b) = do put (0::Word8)
                               put r
                               put g
                               put b
    get = do t <- get :: Get Word8
             case t of
                0 -> do r <- get
                        g <- get
                        b <- get
                        return (PixelRGB8 r g b)
                _ -> return (PixelRGB8 0 0 0)

