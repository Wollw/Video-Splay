module Splay.Image(image) where

import Splay.Options
import Splay.Util

import Codec.Picture

import Control.Concurrent.ParallelIO
import Control.Monad

import Data.Binary
import Data.List (filter, sortBy)
import Data.List.Split (sepBy)
import Data.Map.Strict (lookup, fromList, Map)
import Data.Maybe (fromJust)

import System.Directory
import Text.Printf (printf)

image :: MainOptions -> ImageOptions -> [String] -> IO ()
image mainOpts opts _ = do
    -- Get frame size
    frameCount <- length <$> Data.List.filter isSplayFile
          <$> getDirectoryContents inputDir
    when verbose $ print frameCount
    
    -- Get frame count
    len <- length <$> (decodeFile $ inputDir ++ "/" ++ "x000000000y000000000.splay" :: IO [PixelRGB8])
    when verbose $ print len

    createDirectoryIfMissing True outputDir
    parallel_ [ makeImage width (len `div` width +1) n
              | n <- frameNumbers $ frameCount - 1 ]
  where
    bgColor :: PixelRGB8
    bgColor = (\(r:g:b:[]) -> PixelRGB8 r g b)
            $ ((map read $ sepBy "," (optImageBg opts)) :: [Word8])
    frameNumbers n = sortBy (\n' m' -> (n' `mod` 30) `compare` (m' `mod` 30)) [0..n-1]
    isSplayFile s = (take 6 . reverse) s == reverse ".splay"
    width = optImageWidth opts
    inputDir  = optImageInput opts
    outputDir = optImageOutput opts
    verbose   = optVerbose mainOpts
    makeImage w h n = do
        putStrLn (show n)
        dir <- Data.List.filter isSplayFile <$> getDirectoryContents inputDir
        ps <- decodeFile $ inputDir ++ "/" ++ dir !! n :: IO [PixelRGB8]
        i <- withImage w h $ \x y -> do
            return $ ((ps ++ repeat bgColor) !! (y*width + x))
        writePng (printf "%s/%09d.png" outputDir n :: String) i
