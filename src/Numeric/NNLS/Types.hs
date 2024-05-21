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
import qualified Data.List as L
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.Vector.Storable as VS

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
-- | The machine precision of a Double: @eps = 2.22044604925031e-16@ (the value used by GNU-Octave).
eps :: Double
eps =  2.22044604925031e-16

defaultActiveSetConfig :: ActiveSetConfiguration
defaultActiveSetConfig = ActiveSetConfiguration SolveLS eps 1000

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

checkConstraints' :: Double -> IC -> LA.Vector Double -> Either Text ()
checkConstraints' eps' (IC g h) x = traverse_ checkOne [0..(LA.size x - 1)] where
  gRows = LA.toRows g
  checkOne k =
    let
      gRowk = gRows L.!! k
      hk = h VS.! k
      gRowk_x = gRowk `LA.dot` x
    in case gRowk_x >= hk - eps' || gRowk_x >= hk + eps' of
      True -> pure ()
      False -> Left $ "checkConstraints failed: x=" <> show x
               <> "; G[" <> show k <> ",]=" <> show gRowk
               <> "; h[" <> show k <> "]=" <> show hk
               <> "; G[" <> show k <> ",]x = " <> show gRowk_x <> " < " <> show hk

checkConstraints :: Double -> InequalityConstraints -> LA.Vector Double -> Either Text ()
checkConstraints eps' (SimpleBounds l u) x = do
  checkConstraints' eps' (IC (LA.ident $ LA.size x) l) x
  checkConstraints' eps' (IC (negate $ LA.ident $ LA.size x) l) $ negate x
checkConstraints eps' (MatrixUpper g h) x = checkConstraints eps' (MatrixLower (negate g) (negate h)) x
checkConstraints eps' (MatrixLower g h) x = checkConstraints' eps' (IC g h) x

normDiff ::  LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> Double
normDiff a b x = LA.norm_2 $ (a LA.#> x) - b

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
