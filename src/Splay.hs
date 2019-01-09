module Splay 
    ( sortMethods
    , combineMethods
    , splayMethods
) where

import Splay.Options
import Splay.Util

import Data.Map.Strict

splayMethods = undefined

sortMethods :: SortOptions -> Map String (PixelRGB8 -> PixelRGB8 -> Ordering)
sortMethods o = fromList
    [ ( "default", compare)
    , ( "red",   \(PixelRGB8 r1 _ _) (PixelRGB8 r2 _ _) -> r1 `compare` r2 )
    , ( "green", \(PixelRGB8 _ g1 _) (PixelRGB8 _ g2 _) -> g1 `compare` g2 )
    , ( "blue",  \(PixelRGB8 _ _ b1) (PixelRGB8 _ _ b2) -> b1 `compare` b2 )
    , ( "intersperse", \(PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) -> ((r1 + g1 + b1) `mod` modNum)
                                                            `compare` ((r2 + g2 + b2) `mod` modNum))
    , ( "intersperse-red"
      , \(PixelRGB8 r1 _ _) (PixelRGB8 r2 _ _) -> (r1 `mod` modNum) `compare` (r2 `mod` modNum))
    , ( "intersperse-green"
      , \(PixelRGB8 _ g1 _) (PixelRGB8 _ g2 _) -> (g1 `mod` modNum) `compare` (g2 `mod` modNum))
    , ( "intersperse-blue"
      , \(PixelRGB8 _ _ b1) (PixelRGB8 _ _ b2) -> (b1 `mod` modNum) `compare` (b2 `mod` modNum))
    , ( "difference-red-green"
      , \(PixelRGB8 r1 g1 _) (PixelRGB8 r2 g2 _) -> abs (r1 - g1) `compare` abs (r2 - g2))
    , ( "difference-red-blue"
      , \(PixelRGB8 r1 _ b1) (PixelRGB8 r2 _ b2) -> abs (r1 - b1) `compare` abs (r2 - b2))
    , ( "difference-green-blue"
      , \(PixelRGB8 _ g1 b1) (PixelRGB8 _ g2 b2) -> abs (g1 - b1) `compare` abs (g2 - b2))
    ] where
        modNum = optSortModNum o

-- Custom combine methods.  Either implemented inline using the
-- matching the type signature contained in a Just, or
-- assumed present in the Combine class and registered in its
-- own local combine method lookup
--combineMethods :: CombineOptions -> Map String (Maybe (PixelRGB8 -> PixelRGB8 -> Ordering))
combineMethods o = fromList
    [ ("default",  Nothing) -- Create frames using normal pixel locations from splay files.
    ]

