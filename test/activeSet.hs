{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as VS
import Numeric.ActiveSet
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
      config = ActiveSetConfiguration SolveSVD StartZero 1e-15 1000 LogOnError
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
        if LA.rank m < cols
        then randomNonSingularM rows cols
        else pure m
      randomVector cols = LA.flatten <$> LA.randn cols 1

  let randomNNLS :: Int -> Int -> IO (Either Text (LA.Vector Double), Int)
      randomNNLS n m = do
        a' <- randomNonSingularM n m
        b' <- randomVector n
        optimalNNLS logF config a' b'

      timeIt :: Text -> IO a -> IO a
      timeIt t ma = do
        start <- getCPUTime
        a' <- ma
        end <- getCPUTime
        let diffTime :: Double = fromIntegral(end - start) / 10^(12 :: Int)
        putTextLn $ t <> " took " <> toText @String (printf "%0.3f" diffTime <> "s")
        pure a'

  let nnlsRows :: Int = 40
      nnlsCols :: Int = 15
      nnlsTrials :: Int = 2000
  randomNNLS 40 15 >>= putTextLn . show

  res <- timeIt (show nnlsTrials <> " trials of " <> show nnlsRows <> " x " <> show nnlsCols <> " NNLS")
    (sequence . fmap fst <$> traverse (const $ randomNNLS nnlsRows nnlsCols) [1..nnlsTrials])
  case res of
    Left msg -> putTextLn $ "Error in NNLS trials: " <> msg
    Right _ -> putTextLn $ "NNLS trials all succeeded"


{-
  randomLDP n m = do
    g <- LA.rand n m
    h <- LA.randomVector m
    optimalLDP logF config (MatrixUpper g h) >> showResult a b

  randomLSI n m j c = do
    a <- LA.rand n m
    b <- LA.randomVector m
    g <- LA.rand c m
    h <- LA.randomVector c
    optimalLSI logF config (Original a) b (MatrixUpper g h)
-}
