{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)
    , ChainUpdate (..)

    -- * Helpers
    , waitForConnection
    , defaultRetryPolicy

    -- * Errors
    , ErrNetworkUnavailable (..)
    , ErrNetworkTip (..)
    , ErrGetBlock (..)
    , ErrPostTx (..)
    , ErrDecodeExternalTx (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), Tx, TxWitness )
import Control.Exception
    ( Exception (..), throwIO )
import Control.Monad.Trans.Except
    ( ExceptT )
import Control.Retry
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )

-- | A chain update for the wallet.
data ChainUpdate t
    = RollForward [Block (Tx t)]
    -- ^ New blocks to be processed. This will be a non-empty list of blocks, in
    -- slot order, that are after the current point.
    | RollBackward BlockHeader
    -- ^ The network backend tells us to roll back our state to the given point.
    --
    -- This may be rolling back further than _k_ blocks (if the node chaindb is
    -- deleted for example).
    --
    -- But the wallet is not required to roll back its state further than _k_,
    -- nor to apply blocks older than _k_.

data NetworkLayer m t = NetworkLayer
    { onChainUpdate :: forall a. BlockHeader -> (BlockHeader -> ChainUpdate t -> m ()) -> m a
        -- ^ Register a callback for when new blocks arrive after the given
        -- starting point. The callback will be given the current network tip
        -- (for reporting progress) and a chain update for the wallet. This
        -- function never returns.

    , postTx :: (Tx t, [TxWitness]) -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , decodeExternalTx
        :: ByteString -> ExceptT ErrDecodeExternalTx m (Tx t, [TxWitness])
        -- ^ Decode an externally-signed transaction to the chain producer
    }

-- | Network is unavailable
data ErrNetworkUnavailable
    = ErrNetworkUnreachable Text
      -- ^ Cannot connect to network backend.
    | ErrNetworkInvalid Text
      -- ^ Network backend reports that the requested network is invalid.
    deriving (Generic, Show, Eq)

-- | Exception predicate for 'ErrNetworkUnreachable'.
isNetworkUnreachable :: ErrNetworkUnavailable -> Bool
isNetworkUnreachable (ErrNetworkUnreachable _) = True
isNetworkUnreachable (ErrNetworkInvalid _) = False

-- | Error while trying to get the network tip
data ErrNetworkTip
    = ErrNetworkTipNetworkUnreachable ErrNetworkUnavailable
    | ErrNetworkTipNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkTip

-- | Error while trying to get one or more blocks
data ErrGetBlock
    = ErrGetBlockNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockNotFound (Hash "BlockHeader")
    deriving (Show, Eq)

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxNetworkUnreachable ErrNetworkUnavailable
    | ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance Exception ErrPostTx

-- | Error while trying to decode externally signed transaction
data ErrDecodeExternalTx
    = ErrDecodeExternalTxWrongPayload Text
    | ErrDecodeExternalTxNotSupported
    deriving (Generic, Show, Eq)

instance Exception ErrDecodeExternalTx

-- | Wait until 'networkTip networkLayer' succeeds according to a given
-- retry policy. Throws an exception otherwise.
waitForConnection
    :: NetworkLayer IO t
    -> RetryPolicyM IO
    -> IO ()
waitForConnection nw policy = do
    r <- retrying policy shouldRetry (const $ pure $ Right ()) -- (const $ runExceptT (networkTip nw))
    case r of
        Right _ -> return ()
        Left e -> throwIO e
  where
    shouldRetry _ = \case
        Right _ ->
            return False
        Left ErrNetworkTipNotFound ->
            return True
        Left (ErrNetworkTipNetworkUnreachable e) ->
            return $ isNetworkUnreachable e

-- | A default 'RetryPolicy' with a constant delay, but retries for no longer
-- than a minute.
defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy =
    limitRetriesByCumulativeDelay (60 * second) (constantDelay (1 * second))
  where
    second = 1000*1000
