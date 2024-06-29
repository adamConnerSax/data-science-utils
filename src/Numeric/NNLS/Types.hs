{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Numeric.NNLS.Types where

import qualified Control.Lens as Lens
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Error as X
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.Vector.Storable as VS
import qualified Control.Monad

data AlgoData a = AlgoData { _algoIters :: !Int, _algoData :: !a} deriving stock (Show)

data NNLS_State a = NNLS_Empty | NNLS_Algo !(AlgoData a)

newtype ASM a m x = ASM { unASM :: X.ExceptT Text (RWS.RWST ActiveSetConfiguration (Seq.Seq Text) (AlgoData a) m) x }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (RWS.MonadReader ActiveSetConfiguration, RWS.MonadWriter (Seq.Seq Text), RWS.MonadState (AlgoData a))

throwASM :: Monad m => Text -> ASM a m x
throwASM ~t = ASM . X.throwE $ t

type LogF m = Text -> m ()

runASM :: forall a m x . Monad m => LogF m -> ActiveSetConfiguration -> a -> (LogF m -> ASM a m x) -> m (Either Text x, Int)
runASM log config a toM =
  let runRWST :: RWS.RWST ActiveSetConfiguration (Seq.Seq Text) (AlgoData a) m y -> m (y, AlgoData a, Seq.Seq Text)
      runRWST rwst = RWS.runRWST rwst config (AlgoData 0 a)
      handleResult :: (Either Text x, AlgoData a, Seq.Seq Text) -> m (Either Text x, Int)
      handleResult (e, ad, sText) = do
        when (cfgLogging config == LogOnError) $ case e of
                 Left _ -> void (traverse_ log sText)
                 Right _ -> pure ()
        pure (e, _algoIters ad)
  in  (runRWST . X.runExceptT . unASM $ toM log) >>= handleResult

data Logging = LogNone | LogAll | LogOnError deriving stock (Show, Eq)

data NNLS_Start = StartZero | StartGS

data ActiveSetConfiguration =
  ActiveSetConfiguration {
  cfgSolver :: EqualityConstrainedSolver
  , cfgStart :: NNLS_Start
  , cfgEpsilon :: Double
  , cfgMaxIters :: Int
  , cfgLogging :: Logging
  }
-- | The machine precision of a Double: @eps = 2.22044604925031e-16@ (the value used by GNU-Octave).
eps :: Double
eps =  2.22044604925031e-16

defaultActiveSetConfig :: ActiveSetConfiguration
defaultActiveSetConfig = ActiveSetConfiguration SolveLS StartZero 1e-12 1000 LogOnError

data InequalityConstraints where
  SimpleBounds :: LA.Vector Double -> LA.Vector Double -> InequalityConstraints
  -- ^ l <= x <= u
  MatrixUpper :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax <= b
  MatrixLower :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax >= b
  deriving stock (Show)
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

isEqual :: Double -> EqualityConstraints -> LA.Vector Double -> Bool
isEqual eps' (EqualityConstraints a b) x = isNothing (VS.find ((> eps') . abs) $ (a #> x) - b)
{-# INLINEABLE isEqual #-}

isLowerBound :: Double -> IC -> LA.Vector Double -> Bool
isLowerBound eps' (IC c d) x = isNothing (VS.find ((< negate eps')) $ (c #> x) - d)
{-# INLINEABLE isLowerBound #-}

checkConstraints' :: Text -> Double -> IC -> LA.Vector Double -> Either Text ()
checkConstraints' t eps' ic@(IC g h) x =
  if isLowerBound eps' ic x  then Right () else Left msg
  where
    ~msg = "checkConstraints " <> t <> " failed: x=" <> show x
           <> "; G=" <> show g
           <> "; h=" <> show h
           <> "; Gx = " <> show (g LA.#> x)
{-# INLINEABLE checkConstraints' #-}

checkConstraints :: Text -> Double -> InequalityConstraints -> LA.Vector Double -> Either Text ()
checkConstraints t eps' (SimpleBounds l u) x = do
  checkConstraints' t eps' (IC (LA.ident $ LA.size x) l) x
  checkConstraints' t eps' (IC (negate $ LA.ident $ LA.size x) u) $ negate x
checkConstraints t eps' (MatrixUpper g h) x = checkConstraints t eps' (MatrixLower (negate g) (negate h)) x
checkConstraints t eps' (MatrixLower g h) x = checkConstraints' t eps' (IC g h) x

normDiff ::  LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> Double
normDiff a b x = LA.norm_2 $ (a LA.#> x) - b

data AS_State = ASFree | ASZero deriving (Show, Eq, Ord)

data LH_WorkingSet = LH_WorkingSet (IM.IntMap AS_State) deriving stock (Show, Eq)

data LH_NNLSWorkingData =
  LH_NNLSWorkingData
  { _lhX :: !(LA.Vector Double)
  , _lhWS :: !LH_WorkingSet
  , _lhAtb :: Maybe (LA.Vector Double)
  , _lhAtA :: Maybe (LA.Matrix Double)
  }

type ASMLH m = ASM LH_NNLSWorkingData m

data NNLSStep a = NNLS_Optimal (LA.Vector Double)
                | NNLS_Error Text
                | NNLS_Continue a

data NNLS_LHContinue = LH_Setup
                     | LH_NewFeasible
                     | LH_TestW (LA.Vector Double)
                     | LH_GaussSeidel Int
                     | LH_NewInfeasible (LA.Vector Double)
                     | LH_UnconstrainedSolve (Maybe (LA.Vector Double, Int))

data EqualityConstrainedSolver = SolveSq | SolveLS | SolveSVD | SolveQR deriving stock (Show, Eq)

Lens.makeLenses ''AlgoData
Lens.makeLenses ''LH_NNLSWorkingData
