{-# LANGUAGE OverloadedStrings #-}
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as VS
import Numeric.ActiveSet

main :: IO ()
main = do
  let logF = putTextLn --const $ pure ()
      showResult (eR, n) = case eR of
        Left msg -> putTextLn $ "Error! After " <> show n <> " iterations: " <> msg
        Right x -> putTextLn $ "Solution=" <> show x <> " took " <> show n <> " iteration(s)."
      config = ActiveSetConfiguration SolveLS 1e-15 10
  let a = LA.matrix 3 [1, 1, 0, 0, 1, 1, 1, 0, 1]
      b = LA.vector [1, 2, -1]
  optimalNNLS logF config a b >>= showResult
  let a2 = LA.matrix 3 [40, 90, -120, 30, -120, 90]
      b2 = LA.vector [67.5, -60]
  optimalNNLS logF config a2 b2 >>= showResult
