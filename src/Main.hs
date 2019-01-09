import Splay.Splay (splay)
import Splay.Sort (sort)
import Splay.Combine (combine)
import Options (subcommand, runSubcommand)

main :: IO ()
main = runSubcommand
    [ subcommand "splay"   Splay.Splay.splay
    , subcommand "sort"    Splay.Sort.sort
    , subcommand "combine" Splay.Combine.combine
    ]

