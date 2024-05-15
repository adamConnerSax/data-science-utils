{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.ActiveSet where

import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.Set as S
import qualified Data.Vector.Storable as VS
import qualified Control.Foldl as FL
import qualified Data.List as L
--import Debug.Trace (trace)

-- after Chen & Ye: https://arxiv.org/pdf/1101.6081
projectToSimplex :: VS.Vector Double -> VS.Vector Double
projectToSimplex y = VS.fromList $ fmap (\x -> max 0 (x - tHat)) yL
  where
    yL = VS.toList y
    n = VS.length y
    sY = sort yL
    t i = (FL.fold FL.sum (L.drop i sY) - 1) / realToFrac (n - i)
    tHat = go (n - 1)
    go 0 = t 0
    go k = let tk = t k in if tk > sY L.!! k then tk else go (k - 1)

data ActiveSetConfiguration =
  ActiveSetConfiguration {
  asSolver :: EqualityConstrainedSolver
  , asEpsilon :: Double
  , asMaxIters :: Int
  }

defaultConfig :: ActiveSetConfiguration
defaultConfig = ActiveSetConfiguration SolveLS 1e-15 1000

findOptimal :: ActiveSetConfiguration
            -> LA.Matrix Double
            -> LA.Vector Double
            -> EqualityConstraints
            -> InequalityConstraints
            -> LA.Vector Double
            -> Either Text (LA.Vector Double)
findOptimal config a b ec ic' x = do
  let ic = convertInequalityConstraints ic'
  _ <- if isFeasible (asEpsilon config) ec ic x then Right () else Left "ActiveSet.findOptimal: Given guess is not a feasible solution!"
  let initialWorkingSet = getWorkingSet (asEpsilon config) ic x
      go _ (OptimalFound x') = Right x'
      go _ (StepError msg) = Left $ "ActiveSet.findOptimal: " <> msg
      go n (NotOptimal ws x') = if n < (asMaxIters config)
                                then go (n + 1) $ activeSetStep config a b ws ec ic x'
                                else Left $ "ActiveSet.findOptimal: maxIters reached: x=" <> show x'
  go 0 $ NotOptimal initialWorkingSet x

  -- trace ("initial working set is" <> show initialWorkingSet) $ go 0 $ NotOptimal initialWorkingSet x

data StepResult = OptimalFound (LA.Vector Double) | NotOptimal WorkingSet (LA.Vector Double) | StepError Text deriving stock (Show)

activeSetStep :: ActiveSetConfiguration -> LA.Matrix Double -> LA.Vector Double -> WorkingSet -> EqualityConstraints -> IC -> LA.Vector Double -> StepResult
activeSetStep config a b ws ec@(EqualityConstraints g h) ic x =
  let  (IC c' d') = workingSetConstraints ws ic
       fullC = g === c'
       fullD = VS.concat [h, d']
       numEC = LA.rows g
       (x', allL) = equalityConstrained (asSolver config) a b fullC fullD
  in if isFeasible (asEpsilon config) ec ic x'
     then if Nothing == VS.find (> 0) (VS.drop numEC allL)
          then OptimalFound x'
          else case largestLambdaIndex ws (VS.drop numEC allL) of
                 Nothing -> StepError "Some lambda is positive but there was an error finding the index of it."
                 Just n -> {- trace ("Removing " <> show n <> " from working set and x'=" <> show x') $-} NotOptimal (removeFromWorkingSet n ws) x'
     else
       let u = x' - x
       in case maxAlpha ic x u of
            Nothing -> StepError "New x is not feasible but there was an error finding alpha such that x + alpha u is."
            Just (alpha, n) ->  let x'' = (x + VS.map (* alpha) u)
                                in {- trace ("Adding " <> show n <> " to working set and x'=" <> show x'') $ -} NotOptimal (addToWorkingSet n ws) x''

data InequalityConstraints where
  SimpleBounds :: LA.Vector Double -> LA.Vector Double -> InequalityConstraints
  -- ^ l <= x <= u
  MatrixUpper :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax <= b
  MatrixLower :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax >= b

emptyInequalityConstraints :: Int -> InequalityConstraints
emptyInequalityConstraints n = MatrixLower (LA.matrix n []) (VS.fromList [])

data EqualityConstraints = EqualityConstraints !(LA.Matrix Double) !(LA.Vector Double) deriving stock (Show)

emptyEqualityConstraints :: Int -> EqualityConstraints
emptyEqualityConstraints n = EqualityConstraints (LA.matrix n []) (VS.fromList [])

data IC = IC (LA.Matrix Double) (LA.Vector Double) deriving stock (Show)

convertInequalityConstraints :: InequalityConstraints -> IC
convertInequalityConstraints (SimpleBounds l u ) = IC a b where
  a = LA.ident (LA.size l) === negate (LA.ident $ LA.size u)
  b = VS.concat [l, negate u]
convertInequalityConstraints (MatrixUpper a b) = IC (negate a) (negate b)
convertInequalityConstraints (MatrixLower a b) = IC a b

isFeasible :: Double -> EqualityConstraints -> IC -> LA.Vector Double -> Bool
isFeasible eps (EqualityConstraints g h) (IC c d) x =  eV && iV where
  eV = (VS.find ((> eps) . abs) $ g #> x - h) == Nothing
  iV = (VS.find ((< -eps)) $ c #> x - d) == Nothing

newtype WorkingSet = WorkingSet { workingSet :: Set Int} deriving stock (Show)

-- given a set of inequality constraints, find active and violated sets for a given vector
getWorkingSet :: Double -> IC -> LA.Vector Double -> WorkingSet
getWorkingSet eps (IC a b) u =  f $ VS.foldl' g (0, mempty, mempty) $ (a #> u) - b
  where
    g (n, a', v') x
      | abs x < eps = (n+ 1, S.insert n a', v') -- active
      | x < -eps = (n + 1, a', S.insert n v') -- violated
      | otherwise = (n + 1, a', v') -- inactive
    f (_, a', _) = WorkingSet a'


addToWorkingSet :: Int -> WorkingSet -> WorkingSet
addToWorkingSet k = WorkingSet . S.insert k . workingSet

removeFromWorkingSet :: Int -> WorkingSet -> WorkingSet
removeFromWorkingSet k = WorkingSet . S.delete k . workingSet

workingSetConstraints :: WorkingSet -> IC -> IC
workingSetConstraints (WorkingSet s) (IC c d) = IC c' d' where
  c' = c LA.?? (LA.Pos (LA.idxs  $ S.toList s), LA.All)
  d' = VS.ifilter (\n _ -> S.member n s) d

mostNegativeLambdaIndex :: WorkingSet -> LA.Vector Double -> Maybe Int
mostNegativeLambdaIndex (WorkingSet s) l = safeIndex smallestWorking (S.toList s) where
  f (n, sIndex, smallest) x = if x < smallest then (n + 1, n, x) else (n + 1, sIndex, smallest)
  (_, smallestWorking, _) = VS.foldl' f (0, 0, 0) l
  safeIndex k l' = if k > length l' - 1 then Nothing else Just (l' L.!! k)

largestLambdaIndex :: WorkingSet -> LA.Vector Double -> Maybe Int
largestLambdaIndex (WorkingSet s) l = safeIndex largestWorking (S.toList s) where
  f (n, sIndex, largest) x = if x > largest then (n + 1, n, x) else (n + 1, sIndex, largest)
  (_, largestWorking, _) = VS.foldl' f (0, 0, 0) l
  safeIndex k l' = if k > length l' - 1 then Nothing else Just (l' L.!! k)

-- LS will work if (A' C') is right invertible (linearly indep rows) and C is also right invertible
-- Otherwise you might need the SVD
data EqualityConstrainedSolver = SolveLS | SolveSVD

-- solve ||Ax - b||_2 s.t. Cx = d and produce Least-Squares x as well as Lagrange multipliers for constraints.
-- These are the KKT equations
-- via G(x,l) = ||Ax - b||_2 + l'(Cx - d)
-- dG/dx = 0 and dG/dl = 0
-- solving |2A'A C'| |x| = |2b'A|
--         |C    0 | |l|   |d|
-- LHS matrix is non-singular but might be over or under-determined
equalityConstrained :: EqualityConstrainedSolver
                    -> LA.Matrix Double
                    -> LA.Vector Double
                    -> LA.Matrix Double
                    -> LA.Vector Double
                    -> (LA.Vector Double, LA.Vector Double)
equalityConstrained solver a b c d = VS.splitAt n $ LA.flatten s  where
  n = LA.cols a
  cRows = LA.rows c
  q = (2 * (LA.tr a) <> a ||| LA.tr c) === (c ||| LA.konst 0 (cRows, cRows))
  y = VS.concat [2 * b <# a, d]
  s = case solver of
    SolveLS -> LA.linearSolveLS q (LA.asColumn y)
    SolveSVD -> LA.linearSolveSVD q (LA.asColumn y)


-- for a given set of inequality constraints, a feasible vector x and a perturbation p, find the maximum r
-- s.t., x + r p is feasible as well as the index which limited it
-- we do this by finding the max r for each element and then choosing the smallest of those
maxAlpha :: IC -> LA.Vector Double -> LA.Vector Double -> Maybe (Double, Int)
maxAlpha (IC a b) x p = result where
  rs = VS.zipWith3 (\ax ap b' -> if ax > b'
                                 then
                                   if ap < 0
                                   then (b' - ax) / ap
                                   else 1
                                 else -1) (a #> x) (a #> p) b
  result = fmap (\(a', b') -> (b', a')) $ viaNonEmpty head $ L.sortOn snd $ filter ((> 0) . snd) $ zip [(0 :: Int)..] $ VS.toList rs
