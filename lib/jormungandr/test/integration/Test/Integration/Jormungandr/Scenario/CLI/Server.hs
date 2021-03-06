{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Faucet
    ( getBlock0HText )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( finally )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( typed )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( createProcess
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( SpecWith, describe, it, runIO )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldReturn )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand (..)
    , collectStreams
    , expectPathEventuallyExist
    , proc'
    , shouldContainT
    , shouldNotContainT
    )
import Test.Integration.Framework.TestData
    ( versionLine )
import Test.Utils.Ports
    ( findPort )

import qualified Data.Text as T

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    block0H <- runIO $ T.unpack <$> getBlock0HText
    describe "SERVER - cardano-wallet serve [SERIAL]" $ do
        it "SERVER - Can start cardano-wallet serve --database" $ \_ -> do
            withTempDir $ \d -> do
                let db = d ++ "/db-file"
                let args =
                        [ "serve", "--database", db, "--genesis-hash", block0H ]
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ _ _ ph -> do
                    expectPathEventuallyExist db
                  `finally` do
                    terminateProcess ph
            threadDelay oneSecond

    describe "DaedalusIPC" $ do
        let defaultArgs nodePort =
                [ commandName @t
                , "serve"
                , "--node-port"
                , show nodePort
                , "--genesis-hash"
                , block0H
                ]

        let filepath = "test/integration/js/mock-daedalus.js"

        it "Should reply with the port --random" $ \ctx -> do
            let scriptArgs = defaultArgs (ctx ^. typed @(Port "node"))
                    ++ ["--random-port"]
            (_, _, _, ph) <- createProcess (proc filepath scriptArgs)
            waitForProcess ph `shouldReturn` ExitSuccess

        it "Should reply with the port --random" $ \ctx -> do
            walletPort <- findPort
            let scriptArgs = defaultArgs (ctx ^. typed @(Port "node"))
                    ++ ["--port", show walletPort]
            (_, _, _, ph) <- createProcess (proc filepath scriptArgs)
            waitForProcess ph `shouldReturn` ExitSuccess

    describe "LOGGING - cardano-wallet serve logging" $ do
        it "LOGGING - Launch can log --verbose" $ \ctx -> do
            let args =
                    ["serve"
                    , "--node-port"
                    , show (ctx ^. typed @(Port "node"))
                    , "--random-port"
                    , "--verbose"
                    , "--genesis-hash"
                    , block0H
                    ]
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (20, 0) process
            out `shouldContainT` versionLine
            -- NOTE:
            -- In theory we could also have:
            --
            --     out `shouldContainT` "Debug"
            --     out `shouldContainT` "Notice"
            --
            -- but in practice, we only have INFO logs on start-up.
            out `shouldContainT` "Info"

        it "LOGGING - Serve --quiet logs Error only" $ \ctx -> do
            let args =
                    ["serve"
                    , "--node-port"
                    , show (ctx ^. typed @(Port "node"))
                    , "--random-port"
                    , "--quiet"
                    , "--genesis-hash"
                    , block0H
                    ]
            let process = proc' (commandName @t) args
            (out, err) <- collectStreams (10, 10) process
            out `shouldBe` mempty
            err `shouldBe` mempty

        it "LOGGING - Serve default logs Info" $ \ctx -> do
            let args =
                    ["serve"
                    , "--node-port"
                    , show (ctx ^. typed @(Port "node"))
                    , "--random-port"
                    , "--genesis-hash"
                    , block0H
                    ]
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (5, 0) process
            out `shouldNotContainT` "Debug"
            out `shouldContainT` versionLine
            out `shouldContainT` "Info"
            -- NOTE:
            -- In theory we could also have:
            --
            --     out `shouldContainT` "Notice"
            --
            -- but in practice, we only have INFO logs on start-up.

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
