{-# LANGUAGE OverloadedStrings #-}
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as VS
import Numeric.ActiveSet

main :: IO ()
main = do
  let logF = putTextLn --const $ pure ()
      showResult :: LA.Matrix Double -> LA.Vector Double -> (Either Text (LA.Vector Double), Int) -> IO ()
      showResult a b (eR, n) = case eR of
        Left msg -> (putTextLn $ "Error! After " <> show n <> " iterations: " <> msg)
        Right x -> do
          putTextLn $ "Solution=" <> show x <> " took " <> show n <> " iteration(s)."
          putTextLn $ "||Ax - b||_2 = " <> show (normDiff a b x)
      config = ActiveSetConfiguration SolveLS 1e-15 10
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
  optimalLSI logF config a b (nnlsConstraint $ LA.cols a)  >>= showResult a b
  putTextLn $ show $ checkConstraints 1e-8 (nnlsConstraint $ LA.cols a) $ LA.vector [0.0,1.4999999999999998,0.0]
  putTextLn $ show $ "normDiff=" <> show (normDiff a b (LA.vector [0.0,1.4999999999999998,0.0]))
--  let ldpA n = LA.ident n
--      ldpb n = VS.replicate n 0
--  optimalLSI logF config (ldpA 3) (ldpb 3) (MatrixUpper a $ LA.vector [-1, -1, -1]) >>= showResult (ldpA 3) (ldpb 3)
