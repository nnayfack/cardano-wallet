{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module allows the wallet to retrieve blocks from a known @Jormungandr@
-- node. This is done by providing a @NetworkLayer@ with some logic building on
-- top of an underlying @JormungandrLayer@ HTTP client.
--
-- It also provides facilities for starting the @jormungandr@ node backend
-- process. The Jormungandr 'NetworkLayer' implementation uses the HTTP REST API
-- of this backend.
--
module Cardano.Wallet.Jormungandr.Network
    (
    -- * Starting the network layer
      JormungandrBackend (..)
    , JormungandrConnParams (..)
    , withNetworkLayer
    , newNetworkLayer

    -- * Launching the node backend
    , JormungandrConfig (..)
    , JormungandrLayer (..)
    , withJormungandr
    , connParamsPort

    -- * Errors
    , ErrGetBlockchainParams (..)
    , ErrGetDescendants (..)
    , ErrStartup (..)
    , ErrUnexpectedNetworkFailure (..)

    -- * Re-export
    , BaseUrl (..)
    , Scheme (..)

    -- * Internal constructors
    , mkRawNetworkLayer
    , mkJormungandrLayer
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.Launcher
    ( Command (..)
    , ProcessHasExited
    , StdStream (..)
    , transformLauncherTrace
    , withBackendProcess
    )
import Cardano.Wallet.Jormungandr.Api
    ( BlockId (..)
    , GetBlock
    , GetBlockDescendantIds
    , GetStakeDistribution
    , GetTipId
    , PostMessage
    , StakeApiResponse (..)
    , api
    )
import Cardano.Wallet.Jormungandr.Binary
    ( ConfigParam (..), Message (..), convertBlock, runGetOrFail )
import Cardano.Wallet.Jormungandr.BlockHeaders
    ( BlockHeaders (..)
    , blockHeadersTip
    , emptyBlockHeaders
    , updateUnstableBlocks
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( genConfigFile, localhostBaseUrl, softTxMaxSize )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    , defaultRetryPolicy
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber, getRandomPort, waitForPort )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , Hash (..)
    , SlotLength (..)
    , TxWitness (..)
    )
import Control.Arrow
    ( left )
import Control.Concurrent.MVar.Lifted
    ( MVar, modifyMVar, newMVar )
import Control.Exception
    ( Exception, bracket )
import Control.Monad
    ( forM, void )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status400 )
import Safe
    ( tailSafe )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , responseBody
    , responseStatusCode
    , runClientM
    )
import Servant.Client.Core
    ( ServantError (..) )
import Servant.Links
    ( Link, safeLink )
import System.Directory
    ( removeFile )
import System.FilePath
    ( (</>) )

import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml

-- | Whether to start Jormungandr with the given config, or to connect to an
-- already running Jormungandr REST API using the given parameters.
data JormungandrBackend
    = UseRunning JormungandrConnParams
    | Launch JormungandrConfig
    deriving (Show, Eq)

-- | Parameters for connecting to a Jormungandr REST API.
data JormungandrConnParams = JormungandrConnParams
    { _genesisHash :: Hash "Genesis"
    , _restApi :: BaseUrl
    } deriving (Show, Eq)

-- | A subset of the Jormungandr configuration parameters, used for starting the
-- Jormungandr node backend.
data JormungandrConfig = JormungandrConfig
    { _stateDir :: FilePath
    , _genesisBlock :: FilePath
    , _secretFile :: FilePath
    , _restApiPort :: Maybe PortNumber
    , _minSeverity :: Severity
    , _outputStream :: StdStream
    } deriving (Show, Eq)

-- | Starts the network layer and runs the given action with a
-- 'NetworkLayer'. The caller is responsible for handling errors which may have
-- occurred while starting the Node.
withNetworkLayer
    :: Trace IO Text
    -- ^ Logging
    -> JormungandrBackend
    -- ^ How Jörmungandr is started.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO Tx J.Block) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayer _ (UseRunning cp) action = withNetworkLayerConn cp action
withNetworkLayer tr (Launch lj) action = withNetworkLayerLaunch tr lj action

withNetworkLayerLaunch
    :: Trace IO Text
    -- ^ Logging of node startup.
    -> JormungandrConfig
    -- ^ Configuration for starting Jörmungandr.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO Tx J.Block) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayerLaunch tr lj action = do
    res <- withJormungandr tr lj $ \cp -> withNetworkLayerConn cp action
    either (action . Left) pure res

withNetworkLayerConn
    :: JormungandrConnParams
    -- ^ Parameters for connecting to Jörmungandr node which is already running.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO Tx J.Block) -> IO a)
    -- ^ Action to run with the network layer.
    -> IO a
withNetworkLayerConn cp@(JormungandrConnParams block0H baseUrl) action =
    runExceptT go >>= action . fmap (cp,)
  where
    go = withExceptT ErrStartupGetBlockchainParameters new
    new = newNetworkLayer baseUrl block0H

-- | Creates a new 'NetworkLayer' connecting to an underlying 'Jormungandr'
-- backend target.
newNetworkLayer
    :: BaseUrl
    -> Hash "Genesis"
    -> ExceptT ErrGetBlockchainParams IO (NetworkLayer IO Tx J.Block)
newNetworkLayer baseUrl block0H = do
    mgr <- liftIO $ newManager defaultManagerSettings
    st <- newMVar emptyBlockHeaders
    let jor = mkJormungandrLayer mgr baseUrl
    g0 <- getInitialBlockchainParameters jor (coerce block0H)
    return (mkRawNetworkLayer g0 st jor)

-- | Wrap a Jormungandr client into a 'NetworkLayer' common interface.
--
-- This version provides the full, raw blocks from
-- "Cardano.Wallet.Jormungandr.Binary".
mkRawNetworkLayer
    :: MonadBaseControl IO m
    => (J.Block, BlockchainParameters)
    -> MVar BlockHeaders
    -> JormungandrLayer m
    -> NetworkLayer m Tx J.Block
mkRawNetworkLayer (block0, bp) st j = NetworkLayer
    { networkTip = modifyMVar st $ \bs -> do
        let k = getEpochStability bp
        bs' <- updateUnstableBlocks k getTipId' getBlockHeader bs
        ExceptT . pure $ case blockHeadersTip bs' of
            Just t -> Right (bs', t)
            Nothing -> Left ErrNetworkTipNotFound

    , nextBlocks = \tip -> do
        let count = 10000
        -- Get the descendants of the tip's /parent/.
        -- The first descendant is therefore the current tip itself. We need to
        -- skip it. Hence the 'tail'.
        ids <- tailSafe <$> getDescendantIds j (prevBlockHash tip) count
                `mappingError` \case
            ErrGetDescendantsNetworkUnreachable e ->
                ErrGetBlockNetworkUnreachable e
            ErrGetDescendantsParentNotFound _ ->
                ErrGetBlockNotFound (prevBlockHash tip)
        forM ids (getBlock j)

    , postTx = postMessage j
    , staticBlockchainParameters = (block0, bp)
    }
  where
    getTipId' = (getTipId j) `mappingError`
        ErrNetworkTipNetworkUnreachable
    getBlock' t = (getBlock j t) `mappingError` \case
        ErrGetBlockNotFound _ ->
            ErrNetworkTipNotFound
        ErrGetBlockNetworkUnreachable e ->
            ErrNetworkTipNetworkUnreachable e
    getBlockHeader t = do
        blk@(J.Block blkHeader _) <- getBlock' t
        let nodeHeight = Quantity $ fromIntegral $ J.chainLength blkHeader
        pure (header (convertBlock blk), nodeHeight)

    mappingError = flip withExceptT

{-------------------------------------------------------------------------------
                                Backend launcher
-------------------------------------------------------------------------------}

-- | Launches a Jörmungandr node backend with the given configuration
withJormungandr
    :: Trace IO Text
    -- ^ Logging
    -> JormungandrConfig
    -- ^ Launch configuration
    -> (JormungandrConnParams -> IO a)
    -- ^ Action to run while node is running.
    -> IO (Either ErrStartup a)
withJormungandr tr (JormungandrConfig stateDir block0File secretsFile mPort logSeverity output) cb =
    bracket setupConfig cleanupConfig startBackend
  where
    nodeConfigFile = stateDir </> "jormungandr-config.yaml"
    setupConfig = do
        apiPort <- maybe getRandomPort pure mPort
        p2pPort <- getRandomPort
        let baseUrl = localhostBaseUrl $ fromIntegral apiPort
        genConfigFile stateDir p2pPort baseUrl
            & Yaml.encodeFile nodeConfigFile
        logInfo tr $ mempty
            <> "Generated Jörmungandr's configuration to: "
            <> T.pack nodeConfigFile
        pure (apiPort, baseUrl)
    cleanupConfig _ = removeFile nodeConfigFile

    startBackend (apiPort, baseUrl) = parseBlock0H block0File >>= \case
        Right block0H -> do
            let args =
                    [ "--genesis-block", block0File
                    , "--config", nodeConfigFile
                    , "--secret", secretsFile
                    , "--log-level", C.toLower <$> show logSeverity
                    ]
            let cmd = Command "jormungandr" args (return ()) output
            let tr' = transformLauncherTrace tr
            res <- withBackendProcess tr' cmd $
                waitForPort defaultRetryPolicy apiPort >>= \case
                    True -> Right <$> cb (JormungandrConnParams block0H baseUrl)
                    False -> pure $ Left ErrStartupNodeNotListening
            pure $ either (Left . ErrStartupCommandExited) id res

        Left _ -> pure $ Left $ ErrStartupGenesisBlockFailed block0File

parseBlock0H :: FilePath -> IO (Either ErrStartup (Hash "Genesis"))
parseBlock0H file = parse <$> BL.readFile file
  where
    parse bytes = case runGetOrFail J.getBlockId bytes of
        Right (_, _, block0H) -> Right (coerce block0H)
        Left _ -> Left (ErrStartupGenesisBlockFailed file)

-- | Extract the port number from the base URL part of the connection params.
connParamsPort :: JormungandrConnParams -> Int
connParamsPort (JormungandrConnParams _ url) = baseUrlPort url

{-------------------------------------------------------------------------------
                            Jormungandr Client
-------------------------------------------------------------------------------}

-- | Endpoints of the jormungandr REST API.
data JormungandrLayer m = JormungandrLayer
    { getTipId
        :: ExceptT ErrNetworkUnavailable m (Hash "BlockHeader")
    , getBlock
        :: Hash "BlockHeader"
        -> ExceptT ErrGetBlock m J.Block
    , getDescendantIds
        :: Hash "BlockHeader"
        -> Word
        -> ExceptT ErrGetDescendants m [Hash "BlockHeader"]
    , postMessage
        :: (Tx, [TxWitness])
        -> ExceptT ErrPostTx m ()
    , getInitialBlockchainParameters
        :: Hash "Genesis"
        -> ExceptT ErrGetBlockchainParams m (J.Block, BlockchainParameters)
    , getStakeDistribution
        :: ExceptT ErrNetworkUnavailable m StakeApiResponse
    }

-- | Construct a 'JormungandrLayer'-client
--
-- >>> mgr <- newManager defaultManagerSettings
-- >>> j = mkJormungandrLayer mgr (BaseUrl Http "localhost" 8080 "")
--
-- >>> (Right tip) <- runExceptT $ getTipId j
-- >>> tip
-- BlockId (Hash {getHash = "26c640a3de09b74398c14ca0a137ec78"})
--
-- >>> (Right block) <- runExceptT $ getBlock j t
-- >>> block
-- >>> Block {header = BlockHeader {slotId = SlotId {epochNumber = 0, slotNumber = 0}, prevBlockHash = Hash {getHash = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"}}, transactions = [Tx {inputs = [], outputs = [TxOut {address = Address {unAddress = "3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"}, coin = Coin {getCoin = 14}}]}]}
--
-- At the time of writing, we only have the genesis-block, but we should be
-- able to get its descendants.
--
-- >>> let genesisHash = BlockId (Hash {getHash = "&\198@\163\222\t\183C\152\193L\160\161\&7\236x\245\229\EOT\175\177\167\131\190\b\b/\174\212\177:\179"})
-- >>> runExceptT $ getDescendantIds j t 4
-- Right []
mkJormungandrLayer
    :: Manager -> BaseUrl -> JormungandrLayer IO
mkJormungandrLayer mgr baseUrl = JormungandrLayer
    { getTipId = ExceptT $ do
        let ctx = safeLink api (Proxy @GetTipId)
        run (getBlockId <$> cGetTipId) >>= defaultHandler ctx

    , getBlock = \blockId -> ExceptT $ do
        run (cGetBlock (BlockId blockId)) >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
                return . Left . ErrGetBlockNotFound $ blockId
            x -> do
                let ctx = safeLink api (Proxy @GetBlock) (BlockId blockId)
                left ErrGetBlockNetworkUnreachable <$> defaultHandler ctx x

    , getDescendantIds = \parentId count -> ExceptT $ do
        run (map getBlockId <$> cGetBlockDescendantIds (BlockId parentId) (Just count))  >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
                return . Left $ ErrGetDescendantsParentNotFound parentId
            x -> do
                let ctx = safeLink
                        api
                        (Proxy @GetBlockDescendantIds)
                        (BlockId parentId)
                        (Just count)
                left ErrGetDescendantsNetworkUnreachable <$> defaultHandler ctx x

    -- Never returns 'Left ErrPostTxProtocolFailure'. Will currently return
    -- 'Right ()' when submitting correctly formatted, but invalid transactions.
    --
    -- https://github.com/input-output-hk/jormungandr/blob/fe638a36d4be64e0c4b360ba1c041e8fa10ea024/jormungandr/src/rest/v0/message/post.rs#L25-L39
    , postMessage = \tx -> void $ ExceptT $ do
        run (cPostMessage tx) >>= \case
            Left (FailureResponse e)
                | responseStatusCode e == status400 -> do
                    let msg = T.decodeUtf8 $ BL.toStrict $ responseBody e
                    return $ Left $ ErrPostTxBadRequest msg
            x -> do
                let ctx = safeLink api (Proxy @PostMessage)
                left ErrPostTxNetworkUnreachable <$> defaultHandler ctx x

    , getInitialBlockchainParameters = \block0 -> do
        jblock@(J.Block _ msgs) <- ExceptT $ run (cGetBlock (BlockId $ coerce block0)) >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
                return . Left . ErrGetBlockchainParamsGenesisNotFound $ block0
            x -> do
                let ctx = safeLink api (Proxy @GetBlock) (BlockId $ coerce block0)
                let networkUnreachable = ErrGetBlockchainParamsNetworkUnreachable
                left networkUnreachable <$> defaultHandler ctx x

        let params = mconcat $ mapMaybe getConfigParameters msgs
              where
                getConfigParameters = \case
                    Initial xs -> Just xs
                    _ -> Nothing

        let mpolicy = mapMaybe getPolicy params
              where
                getPolicy = \case
                    ConfigLinearFee x -> Just x
                    _ -> Nothing

        let mduration = mapMaybe getSlotDuration params
              where
                getSlotDuration = \case
                    SlotDuration x -> Just x
                    _ -> Nothing

        let mblock0Date = mapMaybe getBlock0Date params
              where
                getBlock0Date = \case
                    Block0Date x -> Just x
                    _ -> Nothing

        let mepochLength = mapMaybe getSlotsPerEpoch params
              where
                getSlotsPerEpoch = \case
                    SlotsPerEpoch x -> Just x
                    _ -> Nothing

        let mStability = mapMaybe getStability params
              where
                getStability = \case
                   EpochStabilityDepth x -> Just x
                   _ -> Nothing

        case (mpolicy, mduration, mblock0Date, mepochLength, mStability) of
            ([policy],[duration],[block0Date], [epochLength], [stability]) ->
                return
                    ( jblock
                    , BlockchainParameters
                        { getGenesisBlockHash = block0
                        , getGenesisBlockDate = block0Date
                        , getFeePolicy = policy
                        , getEpochLength = epochLength
                        , getSlotLength = SlotLength duration
                        , getTxMaxSize = softTxMaxSize
                        , getEpochStability = stability
                        }
                    )
            _ ->
                throwE $ ErrGetBlockchainParamsIncompleteParams params

    , getStakeDistribution = ExceptT $ do
        let ctx = safeLink api (Proxy @GetStakeDistribution)
        run cGetStakeDistribution >>= defaultHandler ctx
    }
  where
    run :: ClientM a -> IO (Either ServantError a)
    run query = runClientM query (mkClientEnv mgr baseUrl)

    defaultHandler
        :: Link
        -> Either ServantError a
        -> IO (Either ErrNetworkUnavailable a)
    defaultHandler ctx = \case
        Right c -> return $ Right c

        -- The node has not started yet or has exited.
        -- This could be recovered from by either waiting for the node
        -- initialise, or restarting the node.
        Left (ConnectionError e) ->
            return $ Left $ ErrNetworkUnreachable e

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetTipId
        :<|> cGetBlock
        :<|> cGetBlockDescendantIds
        :<|> cPostMessage
        :<|> cGetStakeDistribution
        = client api

data ErrUnexpectedNetworkFailure
    = ErrUnexpectedNetworkFailure Link ServantError
    deriving (Show)

instance Exception ErrUnexpectedNetworkFailure

data ErrGetDescendants
    = ErrGetDescendantsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetDescendantsParentNotFound (Hash "BlockHeader")
    deriving (Show, Eq)

data ErrGetBlockchainParams
    = ErrGetBlockchainParamsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockchainParamsGenesisNotFound (Hash "Genesis")
    | ErrGetBlockchainParamsIncompleteParams [ConfigParam]
    deriving (Show, Eq)

data ErrStartup
    = ErrStartupNodeNotListening
    | ErrStartupGetBlockchainParameters ErrGetBlockchainParams
    | ErrStartupGenesisBlockFailed FilePath
    | ErrStartupCommandExited ProcessHasExited
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

instance Exception ErrStartup
