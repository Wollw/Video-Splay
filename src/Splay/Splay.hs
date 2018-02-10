{-# LANGUAGE BangPatterns #-}
module Splay.Splay where

-- This module uses FFmpeg and JuicyPixels to 
-- 'splay' video files into sequential lists
-- of pixels from the input video.

--import Splay (splayMethods)
import Splay.Options
import Splay.Util

import Codec.FFmpeg
import Codec.Picture

import Control.Concurrent.ParallelIO
import Control.Monad

import Data.Binary
import Data.Map.Strict
import Data.List.Split

import System.Directory
import System.Mem

splay :: MainOptions -> SplayOptions -> [String] -> IO ()
splay mainOpts opts _ = do
    initFFmpeg

    -- Setup output directory
    createDirectoryIfMissing True outputDir

    -- Just needed to get width and height
    (getFrame, cleanup) <- imageReader inputFile
    (Just (ImageRGB8 i)) <- (fmap ImageRGB8 <$> getFrame) <* cleanup

    -- Get and write pixel position over time data
    encodeChunks $ chunksOf chunkSize
        [ (x,y) | x <- [0..imageWidth  i - 1]
                , y <- [0..imageHeight i - 1] ]
    
  where
    chunkSize = optSplayChunkSize opts
    inputFile = optSplayInput opts
    outputDir = optSplayOutput opts
    verbose   = optVerbose mainOpts
    encodeChunks [] = return ()
    encodeChunks (c:cs) = do
        (!ps) <- toList <$> getPixelsFromAt inputFile c
        encodeChunk ps >> performGC
        when verbose $ putStr "Encoded Chunk."
        encodeChunks cs
    encodeChunk !chk =
        parallel_ $ Prelude.map (\((x,y),vs) -> encodeFile (filePath outputDir x y) $ reverse vs) chk


getPixelsFromAt :: FilePath -> [(Int, Int)] -> IO (Map (Int, Int) [PixelRGB8])
getPixelsFromAt inputFile coords = do
    (getFrame, cleanup) <- imageReader inputFile
    getPixels (fmap ImageRGB8 <$> getFrame) <* cleanup
  where
    getPixels gF = do
        frame <- gF
        case frame of
            Just (ImageRGB8 i) -> do
                ps <- getPixels gF
                return . unionWith (++) ps . fromList . flip Prelude.map coords
                 $ \c@(x,y)-> (c, [pixelAt i x y])
            _ -> return empty
