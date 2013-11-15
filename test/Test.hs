module Main where

import Control.Lens
import Control.Monad (when)
import System.IO
import Text.XML.Twiml

{- Say -}

sayExample1 =
  ( respond
  . sayWoman "Hello World"
  $ end
  , "test/xml/sayExample1.xml" )

sayExample2 =
  ( respond
  . (sayAlice' PtBR "Bom dia." <&> loop .~ 2)
  $ end
  , "test/xml/sayExample2.xml" )

sayExamples = [ sayExample1, sayExample2 ]

{- Play -}

-- FIXME: ...
playExample1 =
  ( respond
  . play undefined -- "https://api.twilio.com/cowbell.mp3"
  $ end
  , "test/xml/playExample1.xml" )

playExample2 =
  ( respond
  . (play (undefined) <&> digits .~ [W, W, W, W, D3])
  $ end
  , "test/xml/playExample2.xml" )

playExamples = [ playExample1, playExample2 ]

{- Gather -}

gatherExample1 =
  ( respond
  . gather end
  $ end
  , "test/xml/gatherExample1.xml" )

gatherExample2 =
  ( respond
  . (gather
      (say "Please enter your account number, followed by the pound sign" $ end)
        <&> action .~ undefined
        <&> method .~ GET)
  . say "We didn't receive any input. Goodbye!"
  $ end
  , "test/xml/gatherExample2.xml" )

gatherExamples = [ gatherExample1, gatherExample2 ]

{- Record -}

recordExample1 =
  ( respond
  . record undefined
  $ end
  , "test/xml/recordExample1.xml" )

recordExample2 =
  ( respond
  . say "Please leave a message at the beep. Press the star key when finished."
  . (record undefined <&> action      .~ undefined
                      <&> method      .~ GET
                      <&> maxLength   .~ 20
                      <&> finishOnKey .~ KStar)
  . say "I did not receive a recording"
  $ end
  , "test/xml/recordExample2.xml" )

recordExample3 =
  ( respond
  . (record undefined <&> transcribe         .~ True
                      <&> transcribeCallback .~ undefined)
  $ end
  , "test/xml/recordExample3.xml" )

recordExamples = [ recordExample1, recordExample2, recordExample3 ]

{- Sms -}

smsExample1 =
  ( respond
  . say "Our store is located at 123 Easy St."
  . sms "Store Location: 123 Easy St."
  $ end
  , "test/xml/smsExample1.xml" )

smsExample2 =
  ( respond
  . say "Our store is located at 123 Easy St."
  . (sms "Store Location: 123 Easy St." <&> action .~ undefined
                                        <&> method .~ POST)
  $ end
  , "test/xml/smsExample2.xml" )

smsExample3 =
  ( respond
  . say "Our store is located at 123 Easy St."
  . (sms "Store Location: 123 Easy St." <&> statusCallback .~ undefined)
  $ end
  , "test/xml/smsExample3.xml" )

smsExamples = [ smsExample1, smsExample2, smsExample3 ]

{- Dial -}

dialExample1 =
  ( respond
  . dial (Right "415-123-4567")
  . say "Goodybye"
  $ end
  , "test/xml/dialExample1.xml" )

dialExample2 =
  ( respond
  . (dial (Right "415-123-4567") <&> action .~ undefined
                                 <&> method .~ GET)
  . say "I am unreachable"
  $ end
  , "test/xml/dialExample2.xml" )

dialExample3 =
  ( respond
  . (dial
        (Left $ Number (NumberAttributes Nothing Nothing Nothing) "+1558675309")
      <&> callerId .~ "+15551112222")
  $ end
  , "test/xml/dialExample3.xml" )

dialExamples = [ dialExample1, dialExample2, dialExample3 ]

-- TODO: Dial nouns...

{- Enqueue -}

{-
enqueueExample1 =
  ( respond
  . (enqueue "support" <&> waitURL .~ undefined)
  $ end
  , "test/xml/enqueueExample1.xml" )
-}

enqueueExamples = []

{- Leave -}

leaveExample1 =
  ( respond
  . leave
  $ end
  , "test/xml/leaveExample1.xml" )

leaveExamples = [ leaveExample1 ]

{- Hangup -}

hangupExample1 =
  ( respond
  . hangup
  $ end
  , "test/xml/hangupExample1.xml" )

hangupExamples = [ hangupExample1 ]

{- Redirect -}

redirectExample1 =
  ( respond
  . dial (Right "415-123-4567")
  . redirect undefined
  $ end
  , "test/xml/redirectExample1.xml" )

redirectExample2 =
  ( respond
  . redirect undefined
  $ end
  , "test/xml/redirectExample2.xml" )

redirectExamples = [ redirectExample1, redirectExample2 ]

{- Reject -}

rejectExample1 =
  ( respond
  . reject
  $ end
  , "test/xml/rejectExample1.xml" )

rejectExample2 =
  ( respond
  . (reject <&> reason .~ Busy)
  $ end
  , "test/xml/rejectExample2.xml" )

rejectExamples = [ rejectExample1, rejectExample2 ]

{- Pause -}

pauseExample1 =
  ( respond
  . say "I will pause 10 seconds starting now!"
  . (pause <&> Text.XML.Twiml.length .~ 10)
  . say "I just paused 10 seconds"
  $ end
  , "test/xml/pauseExample1.xml" )

pauseExample2 =
  ( respond
  . (pause <&> Text.XML.Twiml.length .~ 5)
  . say "Hi there."
  $ end
  , "test/xml/pauseExample2.xml" )

pauseExamples = [ pauseExample1, pauseExample2 ]

{- Main -}

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

main :: IO ()
main = do
  results <- sequence $ map check examples
  when (not $ and results) $ error "Failed"

check :: (Response, FilePath) -> IO Bool
check (twiml, filePath) = do
  let a = show twiml
  b <- readFile filePath
  let equal = a == b
  if (not equal)
    then do
      putStrLn $ "Failed: " ++ filePath
      putStrLn $ "Expected:\n" ++ b
      putStrLn $ "Got:\n" ++ a ++ "\n"
    else do
      putStrLn $ "Passed: " ++ filePath
  return equal
