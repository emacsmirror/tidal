module Tidal.CoreB where

import Criterion.Main (Benchmark, bench, bgroup, nf, whnf)
import Sound.Tidal.Core
  ( append,
    cat,
    fastAppend,
    fastCat,
    fastFromList,
    fromList,
    overlay,
    stack,
    timeCat,
  )
import Sound.Tidal.Pattern (toTime, _fast)
import Tidal.Inputs
  ( catPattBig,
    catPattMed,
    catPattMedB,
    catPattSmall,
    pattApp1,
    pattApp2,
    timeCatBig,
    timeCatMed,
    xs3,
    xs4,
    xs5,
    xs6,
  )

_fastB :: [Benchmark]
_fastB =
  [ bgroup
      "_fast"
      [ bench "_fast < 0" $ whnf (_fast (-2)) pattApp2,
        bench "_fast > 0" $ whnf (_fast (toTime $ (10 :: Int) ^ (6 :: Int))) (cat catPattBig)
      ]
  ]

concatB :: [Benchmark]
concatB =
  [ bgroup
      "concat"
      [ bench "fastCat 10^3" $ whnf fastCat catPattSmall,
        bench "fastCat 10^4" $ whnf fastCat catPattMed,
        bench "fastCat 10^5" $ whnf fastCat catPattMedB,
        bench "fastCat 10^6" $ whnf fastCat catPattBig,
        bench "timeCat 10^5" $ whnf timeCat timeCatMed,
        bench "timeCat 10^6" $ whnf timeCat timeCatBig
      ]
  ]

fromListB :: [Benchmark]
fromListB =
  [ bgroup
      "fromList"
      [ bench "fromList" $ whnf fromList xs6,
        bench "fromList nf" $ nf fromList xs6,
        bench "fastFromList 10^3" $ whnf fastFromList xs3,
        bench "fastFromList 10^4" $ whnf fastFromList xs4,
        bench "fastFromList 10^5" $ whnf fastFromList xs5,
        bench "fastFromList 10^6" $ whnf fastFromList xs6,
        bench "fastFromList 10^6 nf" $ nf fastFromList xs6
      ]
  ]

appendB :: [Benchmark]
appendB =
  [ bgroup
      "append"
      [ bench "append" $ whnf (append pattApp1) pattApp2,
        bench "fastAppend" $ whnf (fastAppend pattApp1) pattApp2
      ]
  ]

stackB :: [Benchmark]
stackB =
  [ bgroup
      "stack"
      [ bench "overlay" $ whnf (overlay pattApp1) pattApp2,
        bench "stack" $ whnf stack catPattBig
      ]
  ]
