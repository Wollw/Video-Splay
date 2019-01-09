module Splay.Options where

import Options
import Data.Binary (Word8)

newtype MainOptions = MainOptions { optVerbose :: Bool }
instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "verbose" False "Toggle verbose output."

data SplayOptions = SplayOptions { optSplayInput  :: String
                                 , optSplayOutput :: String
                                 , optSplayChunkSize :: Int
                                 }
instance Options SplayOptions where
  defineOptions = pure SplayOptions
    <*> simpleOption "input"      "" "Video file to splay."
    <*> simpleOption "output"     "" "Output dir for splay files."
    <*> simpleOption "chunk-size" 32 "Number of pixel locations to access at a time.  Decrease if you are running out of memory, increase if you have memory to spare."

data SortOptions = SortOptions { optSortInput     :: String
                               , optSortOutput    :: String
                               , optSortMethod    :: String
                               , optSortChunkSize :: Int
                               , optSortModNum    :: Word8
                               }
instance Options SortOptions where
  defineOptions = pure SortOptions
    <*> simpleOption "input"      "" "Directory with splay files."
    <*> simpleOption "output"     "" "Output directory for sorted splay files."
    <*> simpleOption "method"    "default" "Comparison method."
    <*> simpleOption "chunk-size" 32 "Number of files to operate on at a time"
    <*> simpleOption "mod" 64 "Number to modulo for sorts that use it like interspersed sorts."

data CombineOptions = CombineOptions { optCombineInput  :: String
                                     , optCombineOutput :: String
                                     , optCombineMethod :: String
                                     }
instance Options CombineOptions where
  defineOptions = pure CombineOptions
    <*> simpleOption "input"  "" "Directory with splay files."
    <*> simpleOption "output" "" "Output directory for combined images."
    <*> simpleOption "method" "default" "The combination method to use."

data ImageOptions = ImageOptions { optImageInput  :: String
                                 , optImageOutput :: String
                                 , optImageWidth  :: Int
                                 , optImageBg     :: String
                                 }
instance Options ImageOptions where
  defineOptions = pure ImageOptions
    <*> simpleOption "input"  "" "Directory with splay files."
    <*> simpleOption "output" "" "Output directory for combined images."
    <*> simpleOption "width" 800 "The width of the output images in pixels."
    <*> simpleOption "bg" "0,0,0" "The rgb background color for filled in end pixels."
