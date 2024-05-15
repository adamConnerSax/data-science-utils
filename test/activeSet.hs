import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as VS
import Numeric.ActiveSet

main :: IO ()
main = do
  let a = LA.matrix 3 [1, 1, 0, 0, 1, 1, 1, 0, 1]
      b = LA.vector [1, 1, -1]
      cp = LA.ident 3
      dp = LA.vector [0, 0, 0]
      ec = emptyEqualityConstraints (LA.cols a)
      ic = MatrixLower cp dp
      x = LA.vector [0.5, 0.5, 0.5]
      config = ActiveSetConfiguration SolveSVD 1e-15 1000
--  putTextLn $ show $ activeSetStep eps a b ws ec ic' x
  putTextLn $ show $ findOptimal config a b ec ic x
