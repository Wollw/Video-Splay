{-# LANGUAGE BangPatterns #-}
module Splay.Sort (Splay.Sort.sort) where

import Splay (sortMethods)
import Splay.Options
import Splay.Util

import Control.Concurrent.ParallelIO
import Control.Monad

import Data.Binary
import Data.Map.Strict
import Data.List
import Data.List.Split


import System.Directory

sort :: MainOptions -> SortOptions -> [String] -> IO ()
sort mainOpts opts _ = do
    fs <- Data.List.filter isSplayFile
           <$> getDirectoryContents inputDir
    createDirectoryIfMissing True outputDir
    _ <- case Data.Map.Strict.lookup compFunc $ sortMethods opts of
        Nothing -> putStrLn "Invalid comparison function."
        Just f  -> encodeFilesWith f $ chunksOf chunkSize fs
    return ()
  where
    isSplayFile s = (take 6 . reverse) s == reverse ".splay"
    inputDir  = optSortInput opts
    outputDir = optSortOutput opts
    compFunc  = optSortMethod opts
    verbose   = optVerbose mainOpts
    chunkSize = optSortChunkSize opts
    encodeFilesWith _ [] = return ()
    encodeFilesWith comp (fs:fss) = do
        parallel_
              [ do (!ps) <- decodeFile (inputDir ++ "/" ++ f) :: IO [PixelRGB8]
                   when verbose . putStrLn $ "Writing " ++ f
                   encodeFile (outputDir ++ "/" ++ f)
                    $ Data.List.sortBy comp ps
              | f <- fs ]
        encodeFilesWith comp fss

