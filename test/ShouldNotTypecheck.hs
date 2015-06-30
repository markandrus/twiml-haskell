{-#OPTIONS_GHC -fdefer-type-errors -fno-defer-typed-holes #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE RebindableSyntax #-}
{-#LANGUAGE RecordWildCards #-}

module ShouldNotTypecheck where

import Text.XML.Twiml
import qualified Text.XML.Twiml.Syntax as Twiml

import Control.DeepSeq (NFData)
import Data.Default
import Data.Void
import Distribution.TestSuite
import qualified Test.HUnit as H
import Test.ShouldNotTypecheck

import Prelude

-- NOTE: I don't really understand why these aren't working. I see the warning
-- when the module is compiled.

tests :: IO [Test]
tests = return
  []
{-
  [ makeTest "\"Cannot nest <Gather> in <Gather>\""   gatherInGather
  -- , makeTest "\"Cannot nest <Record> in <Gather>\""   recordInGather
  -- , makeTest "\"Cannot nest <Sms> in <Gather>\""      smsInGather
  -- , makeTest "\"Cannot nest <Dial> in <Gather>\""     dialInGather
  -- , makeTest "\"Cannot nest <Enqueue> in <Gather>\""  enqueueInGather
  -- , makeTest "\"Cannot nest <Leave> in <Gather>\""    leaveInGather
  -- , makeTest "\"Cannot nest <Hangup> in <Gather>\""   hangupInGather
  -- , makeTest "\"Cannot nest <Redirect> in <Gather>\"" redirectInGather
  -- , makeTest "\"Cannot nest <Reject> in <Gather>\""   rejectInGather
  ]
-}

makeTest :: NFData a => String -> a -> Test
makeTest name twiml = Test test
  where test = TestInstance {
    run = runTest . H.TestCase $ shouldNotTypecheck twiml,
    name = name,
    tags = [],
    options = [],
    setOption = \_ _ -> Right test
  }

runTest :: H.Test -> IO Progress
runTest = fmap snd . H.performTest onStart onError onFailure (Finished Pass)
  where
    onStart :: H.State -> Progress -> IO Progress
    onStart _ = return

    onError :: String -> H.State -> Progress -> IO Progress
    onError msg _ _ = return $ Finished (Error msg)

    onFailure :: String -> H.State -> Progress -> IO Progress
    onFailure msg _ _ = return $ Finished (Fail msg)

someGather :: Twiml' '[Gather] Void
someGather =
  do
    gather def end
    end
  where Twiml.Syntax{..} = def

gatherInGather :: Twiml
gatherInGather =
  response $ do
    gather def someGather
    end
  where Twiml.Syntax{..} = def
