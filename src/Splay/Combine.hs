module Splay.Combine(combine) where

import Splay (combineMethods)
import Splay.Options
import Splay.Util

import Codec.Picture

import Control.Concurrent.ParallelIO
import Control.Monad

import Data.Binary
import Data.List (filter, sortBy)
import Data.Map.Strict (lookup, fromList, Map)
import Data.Maybe (fromJust)

import System.Directory
import Text.Printf (printf)

_localCombineMethods :: CombineOptions -> Map String (MainOptions -> CombineOptions -> IO ())
_localCombineMethods o = fromList
    [ ("default",  _methodDefault) -- Create frames using normal pixel locations from splay files.
    ]


combine :: MainOptions -> CombineOptions -> [String] -> IO ()
combine mainOpts opts _ =
    case Data.Map.Strict.lookup combineMethod $ combineMethods opts of
      Nothing -> putStrLn "Combine method not found."
      (Just Nothing) -> do
        let (Just f) = Data.Map.Strict.lookup combineMethod $ _localCombineMethods opts
        putStrLn $ "Builtin method is: " ++ combineMethod
        f mainOpts opts
      (Just (Just method)) -> putStrLn $ "Custom method is: " ++ combineMethod

  where
    combineMethod = optCombineMethod opts

_methodDefault mainOpts opts = do
    -- Get frame size
    sz@(w,h) <- imageSize
         .  Data.List.filter isSplayFile
        <$> getDirectoryContents inputDir
    when verbose $ print sz

    -- Get frame count
    len <- length <$> (decodeFile $ inputDir ++ "/" ++ "x000000000y000000000.splay" :: IO [PixelRGB8])
    when verbose $ print len

    createDirectoryIfMissing True outputDir
    parallel_ [ makeImage w h n | n <- frameNumbers $ len - 1 ]
  where
    frameNumbers n = sortBy (\n' m' -> (n' `mod` 30) `compare` (m' `mod` 30)) [0..n-1]
    isSplayFile s = (take 6 . reverse) s == reverse ".splay"
    inputDir  = optCombineInput opts
    outputDir = optCombineOutput opts
    verbose   = optVerbose mainOpts
    makeImage w h n = do
        i <- withImage w h $ \x y -> do
            ps <- decodeFile $ filePath inputDir x y :: IO [PixelRGB8]
            --when (x==0) $ putStrLn (show x ++ "," ++ show y)
            return $ ps !! n
        writePng (printf "%s/%09d.png" outputDir n :: String) i
    imageSize :: [FilePath] -> (Int, Int)
    imageSize fs = (read x + 1, read y + 1)
      where
        f = maximum fs
        x = take 9 . tail $ f
        y = take 9 . drop 11 $ f
