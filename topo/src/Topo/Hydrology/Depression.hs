{-# LANGUAGE BangPatterns #-}

-- | Depression filling and sink handling helpers for hydrology.
module Topo.Hydrology.Depression
  ( fillDepressions
  , breachRemainingSinks
  ) where

import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import Topo.Hex (hexNeighborIndices)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Fill land depressions using a priority-flood pass.
fillDepressions
  :: Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
fillDepressions waterLevel gridW gridH elev = runST $ do
  let !n = gridW * gridH
      -- Must remain above routing minimum slope while visually negligible.
      eps = 1e-4 :: Float
  filled <- U.thaw elev
  visited <- UM.replicate n False

  let isBoundary i =
        let !x = i `mod` gridW
            !y = i `div` gridW
        in x == 0 || x == gridW - 1 || y == 0 || y == gridH - 1
      mkSeed i = (elev U.! i, i)
      seeds = [ mkSeed i
              | i <- [0 .. n - 1]
              , isBoundary i || elev U.! i < waterLevel
              ]

  forM_ seeds $ \(_, i) -> UM.write visited i True

  let loop !queue
        | Set.null queue = pure ()
        | otherwise = do
            let !((h, i), queue') = Set.deleteFindMin queue
            queue'' <- foldM
              (\q j -> do
                vis <- UM.read visited j
                if vis
                  then pure q
                  else do
                    UM.write visited j True
                    let !hj = elev U.! j
                        !newH = max hj (h + eps)
                    UM.write filled j newH
                    pure (Set.insert (newH, j) q))
              queue'
              (hexNeighborIndices gridW gridH i)
            loop queue''

  loop (Set.fromList seeds)
  U.freeze filled

-- | Raise shallow isolated land sinks just above their lowest rim tile.
breachRemainingSinks
  :: Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
breachRemainingSinks waterLevel gridW gridH elev =
  let eps = 1e-5 :: Float
      maxBreachDepth = 0.05 :: Float
  in U.imap
      (\i h0 ->
        if h0 < waterLevel
          then h0
          else
            let nbrs = hexNeighborIndices gridW gridH i
                nbrHs = map (elev U.!) nbrs
                hmin = minimum nbrHs
                depth = hmin - h0
            in if h0 <= hmin && depth <= maxBreachDepth
                 then hmin + eps
                 else h0)
      elev
