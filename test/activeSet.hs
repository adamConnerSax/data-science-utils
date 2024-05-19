import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as VS
import Numeric.ActiveSet

main :: IO ()
main = do
  let a = LA.matrix 3 [1, 1, 0, 0, 1, 1, 1, 0, 1]
      b = LA.vector [1, 2, -1]
      config = ActiveSetConfiguration SolveLS 1e-15 10 putTextLn
  result <- optimalNNLS config a b
  putTextLn $ show $ result
  let a2 = LA.matrix 3 [40, 90, -120, 30, 120, -90]
      b2 = LA.vector [67.5, 60]
  result2 <- optimalNNLS config a2 b2
  putTextLn $ show $ result2


{-      cp = LA.ident 3
      dp = LA.vector [0, 0, 0]
      ec = emptyEqualityConstraints (LA.cols a)
      ic = MatrixLower cp dp
      x = LA.vector [0.5, 0.5, 0.5]
-}
--  putTextLn $ show $ activeSetStep eps a b ws ec ic' x
