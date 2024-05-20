{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Numeric.NNLS.Types where

import qualified Control.Lens as Lens
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Error as X
import qualified Data.IntMap as IM
import qualified Numeric.LinearAlgebra as LA

data AlgoData a = AlgoData { _algoIters :: !Int, _algoData :: !a} deriving stock (Show)

newtype ASM a m x = ASM { unASM :: X.ExceptT Text (RWS.RWST ActiveSetConfiguration () (AlgoData a) m) x }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (RWS.MonadReader ActiveSetConfiguration, RWS.MonadState (AlgoData a))

throwASM :: Monad m => Text -> ASM a m x
throwASM = ASM . X.throwE


type LogF m = Text -> m ()

runASM :: forall a m x . Functor m => LogF m -> ActiveSetConfiguration -> a -> (LogF m -> ASM a m x) -> m (Either Text x, Int)
runASM log config a toM =
  let runRWST :: RWS.RWST ActiveSetConfiguration () (AlgoData a) m y -> m (y, AlgoData a, ())
      runRWST rwst = RWS.runRWST rwst config (AlgoData 0 a)
      mapResult :: (Either Text x, AlgoData a, ()) -> (Either Text x, Int)
      mapResult (e, ad, _) = (e, _algoIters ad)
  in  fmap mapResult . runRWST . X.runExceptT . unASM $ toM log

data ActiveSetConfiguration =
  ActiveSetConfiguration {
  asSolver :: EqualityConstrainedSolver
  , asEpsilon :: Double
  , asMaxIters :: Int
--  , log :: Text -> m ()
  }

defaultActiveSetConfig :: ActiveSetConfiguration
defaultActiveSetConfig = ActiveSetConfiguration SolveLS 1e-15 1000


data AS_State = ASFree | ASZero deriving (Show, Eq, Ord)

data LH_WorkingSet = LH_WorkingSet (IM.IntMap AS_State) deriving stock (Show)

data LH_NNLSWorkingData = LH_NNLSWorkingData { _lhX :: !(LA.Vector Double), _lhWS :: !LH_WorkingSet }

type ASMLH m = ASM LH_NNLSWorkingData m

data NNLSStep a = NNLS_Optimal (LA.Vector Double)
                | NNLS_Error Text
                | NNLS_Continue a

data NNLS_LHContinue = LH_NewFeasible
                     | LH_TestW (LA.Vector Double)
                     | LH_NewInfeasible (LA.Vector Double)
                     | LH_UnconstrainedSolve (Maybe (LA.Vector Double, Int))

data EqualityConstrainedSolver = SolveLS | SolveSVD

Lens.makeLenses ''AlgoData
Lens.makeLenses ''LH_NNLSWorkingData
