{-#LANGUAGE RebindableSyntax #-}
{-#LANGUAGE RecordWildCards #-}

module Test where

import Text.XML.Twiml
import qualified Text.XML.Twiml.Syntax as Twiml

import Control.Monad
import Control.Lens
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Default
import Data.Maybe (fromJust)
import Data.String
import Distribution.TestSuite
import System.IO

import Prelude

{- Say -}

sayExample1 :: Twiml
sayExample1 =
  response $ do
    say "Hello World" def
    end
  where Twiml.Syntax{..} = def

sayExample2 :: Twiml
sayExample2 =
  response $ do
    say "Bom dia." $ def & voice .~ Just (Alice $ Just PtBR)
                         & loop  .~ Just 2
    end
  where Twiml.Syntax{..} = def

sayExamples :: [(Twiml, FilePath)]
sayExamples =
  [ (sayExample1, "test/xml/sayExample1.xml")
  , (sayExample2, "test/xml/sayExample2.xml")
  ]

{- Play -}

playExample1 :: Twiml
playExample1 =
  response $ do
    play (fromJust $ parseURL "https://api.twilio.com/cowbell.mp3") def
    end
  where Twiml.Syntax{..} = def

playExample2 :: Twiml
playExample2 =
  response $ do
    play' Nothing $ def & digits .~ Just [W, W, W, W, D3]
    end
  where Twiml.Syntax{..} = def

playExamples :: [(Twiml, FilePath)]
playExamples =
  [ (playExample1, "test/xml/playExample1.xml")
  , (playExample2, "test/xml/playExample2.xml")
  ]

{- Gather -}

gatherExample1 :: Twiml
gatherExample1 =
  response $ do
    gather with end
    end
  where Twiml.Syntax{..} = def

gatherExample2 :: Twiml
gatherExample2 =
  response $ do
    gather (with & action .~ parseURL "/process_gather.php"
                 & method .~ Just GET) $ do
      say "Please enter your account number, followed by the pound sign" def
      end
    say "We didn't receive any input. Goodbye!" def
    end
  where Twiml.Syntax{..} = def

gatherExamples :: [(Twiml, FilePath)]
gatherExamples =
  [ (gatherExample1, "test/xml/gatherExample1.xml")
  , (gatherExample2, "test/xml/gatherExample2.xml")
  ]

{- Record -}

recordExample1 :: Twiml
recordExample1 =
  response $ do
    record with
    end
  where Twiml.Syntax{..} = def

recordExample2 :: Twiml
recordExample2 =
  response $ do
    say "Please leave a message at the beep. Press the star key when finished." def
    record $ with & action      .~ parseURL "http://foo.edu/handleRecording.php"
                  & method      .~ Just GET
                  & maxLength   .~ Just 20
                  & finishOnKey .~ Just KStar
    say "I did not receive a recording" def
    end
  where Twiml.Syntax{..} = def

recordExample3 :: Twiml
recordExample3 =
  response $ do
    record $ with & transcribe         .~ Just True
                  & transcribeCallback .~ parseURL "/handle_transcribe.php"
    end
  where Twiml.Syntax{..} = def

recordExamples :: [(Twiml, FilePath)]
recordExamples =
  [ (recordExample1, "test/xml/recordExample1.xml")
  , (recordExample2, "test/xml/recordExample2.xml")
  , (recordExample3, "test/xml/recordExample3.xml")
  ]

{- Sms -}

smsExample1 :: Twiml
smsExample1 =
  response $ do
    say "Our store is located at 123 Easy St." def
    sms "Store Location: 123 Easy St." def
    end
  where Twiml.Syntax{..} = def

smsExample2 :: Twiml
smsExample2 =
  response $ do
    say "Our store is located at 123 Easy St." def
    sms "Store Location: 123 Easy St." $ def
            & action .~ parseURL "/smsHandler.php"
            & method .~ Just POST
    end
  where Twiml.Syntax{..} = def

smsExample3 :: Twiml
smsExample3 =
  response $ do
    say "Our store is located at 123 Easy St." def
    sms "Store Location: 123 Easy St." $ def
            & statusCallback .~ parseURL "/smsHandler.php"
    end
  where Twiml.Syntax{..} = def

smsExamples :: [(Twiml, FilePath)]
smsExamples =
  [ (smsExample1, "test/xml/smsExample1.xml")
  , (smsExample2, "test/xml/smsExample2.xml")
  , (smsExample3, "test/xml/smsExample3.xml")
  ]

{- Dial -}

dialExample1 :: Twiml
dialExample1 =
  response $ do
    dial "415-123-4567" def
    say "Goodbye" def
    end
  where Twiml.Syntax{..} = def

dialExample2 :: Twiml
dialExample2 =
  response $ do
    dial "415-123-4567" $ def
             & action .~ parseURL "/handleDialCallStatus.php"
             & method .~ Just GET
    say "I am unreachable" def
    end
  where Twiml.Syntax{..} = def

dialExample3 :: Twiml
dialExample3 =
  response $ do
    dial' (Left $ Number def "+15558675309") $ def
             & callerId .~ Just "+15551112222"
    end
  where Twiml.Syntax{..} = def

dialExamples :: [(Twiml, FilePath)]
dialExamples =
  [ (dialExample1, "test/xml/dialExample1.xml")
  , (dialExample2, "test/xml/dialExample2.xml")
  , (dialExample3, "test/xml/dialExample3.xml")
  ]

-- TODO: Dial nouns...

{- Enqueue -}

enqueueExample1 :: Twiml
enqueueExample1 =
  response $ do
    enqueue "support" $ def & waitURL .~ parseURL "wait-music.xml"
    end
  where Twiml.Syntax{..} = def

enqueueExamples :: [(Twiml, FilePath)]
enqueueExamples =
  [ (enqueueExample1, "test/xml/enqueueExample1.xml")
  ]

{- Leave -}

leaveExample1 :: Twiml
leaveExample1 =
  response $ do
    leave
    end
  where Twiml.Syntax{..} = def

leaveExamples :: [(Twiml, FilePath)]
leaveExamples =
  [ (leaveExample1, "test/xml/leaveExample1.xml")
  ]

{- Hangup -}

hangupExample1 :: Twiml
hangupExample1 =
  response $ do
    hangup
    end
  where Twiml.Syntax{..} = def

hangupExamples :: [(Twiml, FilePath)]
hangupExamples =
  [ (hangupExample1, "test/xml/hangupExample1.xml")
  ]

{- Redirect -}

redirectExample1 :: Twiml
redirectExample1 =
  response $ do
    dial "415-123-4567" def
    redirect (fromJust $ parseURL "http://www.foo.com/nextInstructions") def
    end
  where Twiml.Syntax{..} = def

redirectExample2 :: Twiml
redirectExample2 =
  response $ do
    redirect (fromJust $ parseURL "../nextInstructions") def
    end
  where Twiml.Syntax{..} = def

redirectExamples :: [(Twiml, FilePath)]
redirectExamples =
  [ (redirectExample1, "test/xml/redirectExample1.xml")
  , (redirectExample2, "test/xml/redirectExample2.xml")
  ]

{- Reject -}

rejectExample1 :: Twiml
rejectExample1 =
  response $ do
    reject def
    end
  where Twiml.Syntax{..} = def

rejectExample2 :: Twiml
rejectExample2 =
  response $ do
    reject $ with & reason .~ Just Busy
    end
  where Twiml.Syntax{..} = def

rejectExamples :: [(Twiml, FilePath)]
rejectExamples =
  [ (rejectExample1, "test/xml/rejectExample1.xml")
  , (rejectExample2, "test/xml/rejectExample2.xml")
  ]

{- Pause -}

pauseExample1 :: Twiml
pauseExample1 =
  response $ do
    say "I will pause 10 seconds starting now!" def
    pause $ with & duration .~ Just 10
    say "I just paused 10 seconds" def
    end
  where Twiml.Syntax{..} = def

pauseExample2 :: Twiml
pauseExample2 =
  response $ do
    pause $ with & duration .~ Just 5
    say "Hi there." def
    end
  where Twiml.Syntax{..} = def

pauseExamples :: [(Twiml, FilePath)]
pauseExamples =
  [ (pauseExample1, "test/xml/pauseExample1.xml")
  , (pauseExample2, "test/xml/pauseExample2.xml")
  ]

{- Main -}

examples :: [(Twiml, FilePath)]
examples = concat
  [ sayExamples
  , playExamples
  , gatherExamples
  , recordExamples
  , smsExamples
  , dialExamples
  , enqueueExamples
  , leaveExamples
  , hangupExamples
  , redirectExamples
  , rejectExamples
  ] 

ifThenElse :: Bool -> t -> t -> t
ifThenElse b x y | b = x
                 | otherwise = y

tests :: IO [Test]
tests = return $ map check examples

check :: (Twiml, FilePath) -> Test
check (twiml, filePath) = Test test
  where test = TestInstance {
    run = do
      let a = show twiml
      b <- readFile filePath
      let equal = a == b
      when (not equal) $ do
        putStrLn ""
        putStrLn . ppDiff $ getGroupedDiff (lines b) (lines a)
      return . Finished $ case equal of
        True  -> Pass
        False -> Error "Check preceding diff",
    name = filePath,
    tags = [],
    options = [],
    setOption = \_ _ -> Right test
  }
