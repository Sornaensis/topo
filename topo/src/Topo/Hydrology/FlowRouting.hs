{-# LANGUAGE BangPatterns #-}

-- | Flow-direction and sink-breaching helpers for hydrology routing.
module Topo.Hydrology.FlowRouting
  ( flowDirections
  , flowDirectionsLand
  , breachSinksLand
  , createRiverLakes
  ) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Topo.Hex (hexNeighborIndices)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

neighborMin :: Int -> Int -> U.Vector Float -> Int -> Float
neighborMin gridW gridH elev i =
  let h0 = elev U.! i
      nbrHeights = map (elev U.!) (hexNeighborIndices gridW gridH i)
  in minimum (h0 : nbrHeights)

-- | Compute steepest-descent flow directions using all 6 hex neighbours.
-- Returns @-1@ for sinks.
flowDirections :: Int -> Int -> U.Vector Float -> U.Vector Int
flowDirections gridW gridH elev =
  U.generate (U.length elev) $ \i ->
    let h0 = elev U.! i
        candidates = [(j, elev U.! j) | j <- hexNeighborIndices gridW gridH i]
        lower = filter (\(_, h) -> h < h0) candidates
    in case lower of
         [] -> -1
         _  -> fst (minimumByElevation lower)
  where
    minimumByElevation = foldl1 (\a b -> if snd a <= snd b then a else b)

-- | Like 'flowDirections' but treats submerged tiles as terminal sinks.
flowDirectionsLand :: Float -> Int -> Int -> U.Vector Float -> U.Vector Int
flowDirectionsLand waterLevel gridW gridH elev =
  U.generate (U.length elev) $ \i ->
    let h0 = elev U.! i
    in if h0 < waterLevel
       then -1
       else
         let candidates = [(j, elev U.! j) | j <- hexNeighborIndices gridW gridH i]
             lowerLand = filter (\(_, h) -> h < h0) candidates
         in case lowerLand of
              [] -> -1
              _  -> fst (minimumByElevation lowerLand)
  where
    minimumByElevation = foldl1 (\a b -> if snd a <= snd b then a else b)

-- | Lower shallow land sinks by a fixed breach depth, clamped above sea level.
breachSinksLand
  :: Float
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
breachSinksLand waterLevel breachDepth gridW gridH elev =
  U.generate (U.length elev) $ \i ->
    let !h0 = elev U.! i
        !hmin = neighborMin gridW gridH elev i
        isSink = hmin >= h0
        floorLevel = waterLevel + 1e-5
    in if isSink && h0 > waterLevel
         then max floorLevel (h0 - breachDepth)
         else h0

-- | Expand inland depressions fed by rivers into minimum-size lakes.
createRiverLakes
  :: Float
  -> Float
  -> Int
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Int
  -> U.Vector Float
  -> U.Vector Float
createRiverLakes waterLevel minDischarge minLakeSize gridW gridH elev flow discharge = runST $ do
  let !n = gridW * gridH

  oceanMask <- UM.replicate n False
  do
    queue <- UM.replicate n (0 :: Int)
    headR <- UM.replicate 1 (0 :: Int)
    tailR <- UM.replicate 1 (0 :: Int)
    let enqueue v = do
          t <- UM.read tailR 0
          UM.write queue t v
          UM.write tailR 0 (t + 1)
        dequeue = do
          h <- UM.read headR 0
          t <- UM.read tailR 0
          if h >= t then pure Nothing
          else do
            v <- UM.read queue h
            UM.write headR 0 (h + 1)
            pure (Just v)
    forM_ [0 .. n - 1] $ \i -> do
      let !x = i `mod` gridW
          !y = i `div` gridW
          onBoundary = x == 0 || x == gridW - 1 || y == 0 || y == gridH - 1
      when (onBoundary && elev U.! i < waterLevel) $ do
        UM.write oceanMask i True
        enqueue i
    let bfsLoop = do
          mi <- dequeue
          case mi of
            Nothing -> pure ()
            Just cur -> do
              forM_ (hexNeighborIndices gridW gridH cur) $ \ni -> do
                vis <- UM.read oceanMask ni
                when (not vis && elev U.! ni < waterLevel) $ do
                  UM.write oceanMask ni True
                  enqueue ni
              bfsLoop
    bfsLoop

  compId <- UM.replicate n (-1 :: Int)
  nextLabel <- UM.replicate 1 (0 :: Int)
  stackBuf <- UM.replicate n (0 :: Int)

  forM_ [0 .. n - 1] $ \i -> do
    isOcean <- UM.read oceanMask i
    cid <- UM.read compId i
    when (not isOcean && elev U.! i < waterLevel && cid < 0) $ do
      label <- UM.read nextLabel 0
      UM.write nextLabel 0 (label + 1)
      UM.write compId i label
      UM.write stackBuf 0 i
      let fillLoop !top
            | top < 0 = pure ()
            | otherwise = do
                cur <- UM.read stackBuf top
                let top' = top - 1
                foldM
                  (\t ni -> do
                    isO <- UM.read oceanMask ni
                    c <- UM.read compId ni
                    if not isO && elev U.! ni < waterLevel && c < 0
                      then do
                        UM.write compId ni label
                        let t' = t + 1
                        UM.write stackBuf t' ni
                        pure t'
                      else pure t)
                  top'
                  (hexNeighborIndices gridW gridH cur)
                  >>= fillLoop
      fillLoop 0

  numLabels <- UM.read nextLabel 0
  compSize <- UM.replicate (max 1 numLabels) (0 :: Int)
  compFed <- UM.replicate (max 1 numLabels) False

  frozenCompId <- U.freeze compId
  forM_ [0 .. n - 1] $ \i -> do
    let cid = frozenCompId U.! i
    when (cid >= 0) $ do
      UM.modify compSize (+ 1) cid
      let hasRiverInflow = any
            (\ni -> elev U.! ni >= waterLevel
              && flow U.! ni == i
              && discharge U.! ni >= minDischarge)
            (hexNeighborIndices gridW gridH i)
      when hasRiverInflow $ UM.write compFed cid True

  result <- U.thaw elev
  forM_ [0 .. numLabels - 1] $ \cid -> do
    sz <- UM.read compSize cid
    fed <- UM.read compFed cid
    when (fed && sz < minLakeSize) $ do
      let candidates = Set.fromList
            [ (elev U.! ni, ni)
            | i <- [0 .. n - 1]
            , frozenCompId U.! i == cid
            , ni <- hexNeighborIndices gridW gridH i
            , elev U.! ni >= waterLevel
            , frozenCompId U.! ni < 0
            ]
      let expandLoop !curSz !frontier
            | curSz >= minLakeSize = pure ()
            | Set.null frontier = pure ()
            | otherwise = do
                let !((_h, idx), frontier') = Set.deleteFindMin frontier
                UM.write result idx (waterLevel - 1e-4)
                let newCandidates =
                      [ (elev U.! ni, ni)
                      | ni <- hexNeighborIndices gridW gridH idx
                      , elev U.! ni >= waterLevel
                      , frozenCompId U.! ni < 0
                      ]
                expandLoop (curSz + 1)
                  (foldl (\s p -> Set.insert p s) frontier' newCandidates)
      expandLoop sz candidates

  U.freeze result
