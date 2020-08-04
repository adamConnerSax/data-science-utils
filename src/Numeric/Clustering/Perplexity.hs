module Numeric.Clustering.Perplexity where

import qualified Data.Massiv.Array as MA
import qualified Numeric.NLOPT as NLOPT
import qualified Numeric.LinearAlgebra as LA
import qualified Control.Monad.Catch as X
import qualified Data.Text as Text


pGivenBeta :: MA.Ix1 -> MA.Vector MA.U Double -> Double -> MA.Vector MA.D Double
pGivenBeta rowIndex distances beta =
  let scale ci d = if (ci == rowIndex) then 0 else exp (-d * beta)
      raw = MA.imap scale distances
      sumRaw = MA.sum raw
  in raw MA..* (1 / sumRaw)

entropy :: MA.Vector MA.U Double -> MA.Ix1 -> Double -> Double
entropy distances rowIndex beta = MA.sum $ MA.map (\x -> -x * log x) $ pGivenBeta rowIndex distances beta 

findBeta :: Double -> MA.Ix1 -> MA.Vector MA.U Double -> Either NLOPT.Result NLOPT.Solution
findBeta perplexity rowIndex distances =
  let targetEntropy = log perplexity
      obj x = targetEntropy - entropy distances rowIndex (x `LA.atIndex` 0)
      algo = NLOPT.NEWUOA obj Nothing
      stopWhenAny = NLOPT.ObjectiveAbsoluteTolerance 1e-4 NLOPT.:|
                    [NLOPT.MaximumEvaluations 50
                    ]
      problem = NLOPT.LocalProblem 1 stopWhenAny algo
  in NLOPT.minimizeLocal problem (LA.fromList [1])
  

data SolveException = SolveException Text.Text deriving (Show)
instance X.Exception SolveException
     
probabilities :: MA.MonadThrow m => Double -> MA.Matrix MA.U Double -> m (MA.Matrix MA.U Double)
probabilities perplexity distances = do
  let distanceRows = MA.outerSlices distances
      betaSolsE :: Either NLOPT.Result (MA.Vector MA.D Double) 
      betaSolsE = MA.itraverseA (\ix v -> fmap ((`LA.atIndex` 0) . NLOPT.solutionParams) $ findBeta perplexity ix v) distanceRows
  betas <- case betaSolsE of
    Left res -> X.throwM (SolveException $ Text.pack $ show res)
    Right x -> return x
  fmap MA.compute $ MA.stackOuterSlicesM $ MA.izipWith pGivenBeta distanceRows betas
  
