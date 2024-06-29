{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as VS
import qualified Control.Foldl as FL
import Numeric.ActiveSet
import qualified Numeric.NNLS.LH as LH
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

main :: IO ()
main = do
  let logF = putTextLn --const $ pure ()
      showResult :: LA.Matrix Double -> LA.Vector Double -> (Either Text (LA.Vector Double), Int) -> IO ()
      showResult a b (eR, n) = case eR of
        Left msg -> (putTextLn $ "Error! After " <> show n <> " iterations: " <> msg)
        Right x -> do
          putTextLn $ "Solution=" <> show x <> " took " <> show n <> " iteration(s)."
          putTextLn $ "||Ax - b||_2 = " <> show (normDiff a b x)
      config = ActiveSetConfiguration SolveLS StartZero 1e-15 1000 LogOnError
  let a = LA.matrix 3 [1, 1, 0, 0, 1, 1, 1, 0, 1]
      b = LA.vector [1, 2, -1]
  optimalNNLS logF config a b >>= showResult a b
  putTextLn ""
  let a2 = LA.matrix 3 [40, 90, -120, 30, -120, 90]
      b2 = LA.vector [67.5, -60]
  optimalNNLS logF config a2 b2 >>= showResult a2 b2
  putTextLn ""
  optimalLDP logF config (MatrixUpper a $ LA.vector [-1, -1, -1]) >>= showResult (LA.ident $ LA.cols a) (VS.replicate (LA.cols a) 0)
  putTextLn ""
  let nnlsConstraint n = MatrixLower (LA.ident n) $ VS.replicate n 0
  optimalLSI logF config (Original a) b (nnlsConstraint $ LA.cols a)  >>= showResult a b

-- now some random NNLS problems to test the algo
  let randomNonSingularM :: Int -> Int -> IO (LA.Matrix Double)
      randomNonSingularM rows cols = do
        m <- LA.randn rows cols
        if LA.rank m < min rows cols
          then randomNonSingularM rows cols
          else pure m
      randomVector cols = LA.flatten <$> LA.randn cols 1

  let randomNNLS :: Int -> Int -> IO (Either Text (LA.Vector Double))
      randomNNLS n m = do
        a' <- LA.randn n m
        b' <- randomVector n
        LH.nnlsResE <$> LH.optimalNNLS a' b'

      timeIt :: Text -> IO a -> IO a
      timeIt t ma = do
        start <- getCPUTime
        a' <- ma
        end <- getCPUTime
        let diffTime :: Double = fromIntegral(end - start) / 10^(12 :: Int)
        putTextLn $ t <> " took " <> toText @String (printf "%0.3f" diffTime <> "s")
        pure a'

      nnlsRows :: Int = 60
      nnlsCols :: Int = 100
      nnlsTrials :: Int = 1000


  randomNNLS 40 15 >>= putTextLn . show

  let processResults :: [(Either Text (LA.Vector Double), Int)] -> IO ()
      processResults res = do
        let (es, iters') = unzip res
            e = sequence es
        case e of
          Left msg -> putTextLn $ "Error in NNLS trials: " <> msg
          Right _ -> do
            let avgIters = FL.fold (FL.premap realToFrac FL.mean) iters'
            putTextLn $ "Trials all succeeded. <Iters>=" <> show avgIters


  let processResultsLH :: [Either Text (LA.Vector Double)] -> IO ()
      processResultsLH res = do
        let e = sequence res
        case e of
          Left msg -> putTextLn $ "Error in NNLS trials: " <> msg
          Right _ -> do
            putTextLn $ "Trials all succeeded."

  putStrLn "Random NNLS..."
  resNNLS <- timeIt (show nnlsTrials <> " trials of " <> show nnlsRows <> " x " <> show nnlsCols <> " NNLS")
             $ traverse (const $ randomNNLS nnlsRows nnlsCols) [1..nnlsTrials]
  processResultsLH resNNLS

  let randomLDP :: Int -> Int -> IO (Either Text (LA.Vector Double))
      randomLDP n m = do
        g <- LA.randn n m
        let h = g LA.#> LA.vector (replicate m 1)
        LH.nnlsResE <$> LH.optimalLDP (MatrixUpper g h)

  putStrLn "Random LDP..."
  resLDP <- timeIt (show nnlsTrials <> " trials of " <> show nnlsRows <> " x " <> show nnlsCols <> " LDP")
            $ traverse (const $ randomLDP nnlsRows nnlsCols) [1..nnlsTrials]

  processResultsLH resLDP

  let randomLSI n m c = do
        e <- randomNonSingularM n m
        f <- randomVector n
        g <- LA.randn c m
        let h = g LA.#> LA.vector (replicate m 1)
        LH.nnlsResE <$> LH.optimalLSI' (LH.Original e) f (MatrixUpper g h)

  putStrLn "Random LSI..."
  resLSI <- timeIt (show nnlsTrials <> " trials of " <> show nnlsRows <> " x " <> show nnlsCols <> " LSI")
            $ traverse (const $ randomLSI nnlsRows nnlsCols nnlsRows) [1..nnlsTrials]

  processResultsLH resLSI
  pure ()
