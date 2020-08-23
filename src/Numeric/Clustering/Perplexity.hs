{-# LANGUAGE OverloadedStrings #-}
module Numeric.Clustering.Perplexity where

import qualified Data.Massiv.Array as MA
import qualified Numeric.NLOPT as NLOPT
import qualified Numeric.LinearAlgebra as LA
import qualified Control.Monad.Catch as X
import qualified Data.Text as Text


pGivenBeta :: MA.Source r MA.Ix1 Double => MA.Ix1 -> MA.Vector r Double -> Double -> MA.Vector MA.D Double
pGivenBeta rowIndex distances beta =
  let scale ci d = if (ci == rowIndex) then 0 else exp (-d * beta)
      raw = MA.imap scale distances
      sumRaw = MA.sum raw
  in raw MA..* (1 / sumRaw)

safe_xLogx :: Double -> Double
safe_xLogx x = if x > 1e-7 then x * log x else 0

entropy :: MA.Source r MA.Ix1 Double => MA.Vector r Double -> MA.Ix1 -> Double -> Double
entropy distances rowIndex beta = MA.sum $ MA.map (\x -> negate $ safe_xLogx x) $ pGivenBeta rowIndex distances beta 

findBeta :: MA.Source r MA.Ix1 Double => Double -> MA.Ix1 -> MA.Vector r Double -> Either Text.Text Double
findBeta perplexity rowIndex distances =
  let targetEntropy = log perplexity
      obj x = abs (targetEntropy - entropy distances rowIndex (x `LA.atIndex` 0))
      algo = NLOPT.NELDERMEAD obj [] Nothing
      stopWhenAny = NLOPT.ObjectiveAbsoluteTolerance 1e-4 NLOPT.:|
                    [NLOPT.MaximumEvaluations 50
                    ]
      problem = NLOPT.LocalProblem 1 stopWhenAny algo
      optimizedE = NLOPT.minimizeLocal problem (LA.fromList [1])
      textError sol = (Text.pack $ show $ NLOPT.solutionResult sol)
                      <> ": x = "
                      <> (Text.pack $ show $ NLOPT.solutionParams sol)
                      <> "; obj(x) = "
                      <> (Text.pack $ show $ NLOPT.solutionCost sol)
  in case optimizedE of
    Left r -> Left $ Text.pack $ show r
    Right s -> case NLOPT.solutionResult s of
      NLOPT.FTOL_REACHED -> Right $ (`LA.atIndex` 0) $ NLOPT.solutionParams s
      _ -> Left $ textError s
  
data SolveException = SolveException Text.Text deriving (Show)
instance X.Exception SolveException
     
probabilities :: MA.MonadThrow m => Double -> MA.Matrix MA.U Double -> m (MA.Matrix MA.U Double)
probabilities perplexity distances = do
  let distanceRows = MA.computeAs MA.B $ MA.outerSlices distances
      betaSolsE :: Either Text.Text (MA.Vector MA.U Double) 
      betaSolsE = MA.itraverseA (\ix v -> findBeta perplexity ix v) distanceRows   
  betas <- case betaSolsE of
    Left err -> X.throwM (SolveException err)
    Right x -> return x    
  fmap MA.compute $ MA.stackOuterSlicesM $ MA.izipWith pGivenBeta distanceRows betas
  
