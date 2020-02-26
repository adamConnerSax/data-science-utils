{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Control.DataCache where

import qualified Polysemy                      as P
import qualified Polysemy.KVStore              as P
import qualified Polysemy.State                as P

import qualified Data.Map                      as M
import qualified Control.Monad.Catch           as C

data Serialize e a b where
  Serialize :: (a -> b) -> (b -> Either e a) -> Serialize a b
  
-- Just like KVStore but with an (Either e)
data DataCache e k b m a where
  Retrieve :: k -> DataCache k b m (Either w b)
  Update :: k -> Maybe b -> DataCache k b m (Either e ())

type DataCacheIO = DataCache IO

P.makeSem ''DataCache

eitherThrow :: P.Member (P.Error e) r => Either e a -> P.sem r a
eitherThrow = either P.throw return

store :: P.Members [DataCache e k b, P.Error e] r => Serialize a b -> k -> a -> P.Sem r ()
store (Serialize encode _) k x = eitherThrow $ P.update k (Just $ encode x)

retrieve
  :: P.Members [DataCache e1 k b, P.Error e1, P.Error e2 b] r
  => Serialize e2 a b
  -> k
  -> P.Sem r a
retrieve (Serialize _ decode) k = traverse (eitherThrow . decode) $ eitherThrow $ P.retrieve k

--

clear :: P.Member (P.KVStore k b) r => k -> P.Sem r ()
clear k = P.deleteKV k

exists :: P.Member (P.KVStore k b) r => k -> P.Sem r Bool
exists k = P.existsKV k

data FileIO r k b where
  FileIO :: P.Member (Embed IO) r
         => (FilePath -> k -> P.Sem r (Either T.Text b))
         -> (FilePath -> k -> b -> P.Sem r Bool)
         -> FileIO r k b

runDataCacheIO
  :: P.Member (Embed IO) r
  => FileIO (P.Sem r) b
  -> FilePath
  -> P.Sem (P.KVStore k b ': r) a
  -> P.Sem (P.State (M.Map k b) ': r) a
runDataCacheIO (FileIO read write) dir = P.reinterpret $ \case
  P.LookupKV k -> do
    inMap <- P.gets $ M.lookup k
    case inMap of
      Just x  -> Just x
      Nothing -> either (const Nothing) Just $ read dir k
  P.UpdateKV k mb -> do
    fromMaybe (return ()) (write dir k) mb
    P.modify $ M.alter (const mb) k

{-
data OnDemand r a where
  UnMade :: P.Sem r a -> OnDemand r a
  Made :: a -> OnDemand r a

data OnDemandCache k b m a where
  Retrieve :: k -> OnDemandCache k b m (Maybe b)


make :: OnDemand r a -> P.Sem r (OnDemand r a)
make (UnMade ma) = do
  a <- ma
  return (Made a)
make (Made a) = Made a
-}
