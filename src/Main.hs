import Splay.Splay (splay)
import Splay.Sort (sort)
import Splay.Combine (combine)
import Splay.Image (image)
import Options (subcommand, runSubcommand)

main :: IO ()
main = runSubcommand
    [ subcommand "splay"   Splay.Splay.splay
    , subcommand "sort"    Splay.Sort.sort
    , subcommand "combine" Splay.Combine.combine
    , subcommand "image"   Splay.Image.image
    ]

