{- cabal:
default-language: GHC2021
ghc-options: -threaded
build-depends: base, vty, vty-crossplatform
-}

module Main where

import Control.Monad (replicateM_)

xRepeat = 19
yRepeat = 12

main = replicateM_ yRepeat
    $ (`mapM_` ["/ \\_", "\\_/ "]) 
    $ (>> putStrLn "") . replicateM_ xRepeat . putStr
