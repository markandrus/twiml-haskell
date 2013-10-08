module Test where

import Control.Lens
import System.IO
import Text.XML.Twiml

{- Say -}

sayExample1
  = respond
  . sayWoman' French "Chapeau!"
  $ end

sayExample2
  = respond
  . (sayAlice' PtBR "Bom dia." <&> loop .~ 2)
  $ end

{- Play -}

-- FIXME: ...
playExample1
  = respond
  . play undefined -- "https://api.twilio.com/cowbell.mp3"
  $ end

playExample2
  = respond
  . (play (undefined) <&> digits .~ [W, W, W, W, D3])
  $ end

{- Gather -}

gatherExample1
  = respond
  . gather end
  $ end

gatherExample2
  = respond
  . (gather
      (say "Please enter your account number, followed by the pound sign" $ end)
        <&> action .~ undefined
        <&> method .~ GET)
  . say "We didn't receive any input. Goodybye!"
  $ end

{- Record -}

recordExample1
  = respond
  . record undefined
  $ end

recordExample2
  = respond
  . say "Please leave a message at the beep. Press the star key when finished."
  . (record undefined <&> action      .~ undefined
                      <&> method      .~ GET
                      <&> maxLength   .~ 20
                      <&> finishOnKey .~ KStar)
  . say "I did not receive a recording"
  $ end

recordExample3
  = respond
  . (record undefined <&> transcribe         .~ True
                      <&> transcribeCallback .~ undefined)
  $ end

{- Sms -}

smsExample1
  = respond
  . say "Our store is located at 123 Easy St."
  . sms "Store Location: 123 Easy St."
  $ end

smsExample2
  = respond
  . say "Our store is located at 123 Easy St."
  . (sms "Store Location: 123 Easy St." <&> action .~ undefined
                                        <&> method .~ POST)
  $ end

smsExample3
  = respond
  . say "Our store is located at 123 Easy St."
  . (sms "Store Location: 123 Easy St." <&> statusCallback .~ undefined)
  $ end

{- Dial -}

dialExample1
  = respond
  . dial (Right "415-123-4567")
  . say "Goodybye"
  $ end

dialExample2
  = respond
  . (dial (Right "415-123-4567") <&> action .~ undefined
                                 <&> method .~ GET)
  . say "I am unreachable"
  $ end

dialExample3
  = respond
  . (dial
        (Left $ Number (NumberAttributes Nothing Nothing Nothing) "+1558675309")
      <&> callerId .~ "+15551112222")
  $ end

-- TODO: Dial nouns...

{- Enqueue -}

{-
enqueueExample1
  = respond
  . (enqueue "support" <&> waitURL .~ undefined)
  $ end
-}

{- Leave -}

leaveExample1
  = respond
  $ leave

{- Hangup -}

hangupExample1
  = respond
  $ hangup

{- Redirect -}

redirectExample1
  = respond
  . dial (Right "415-123-4567")
  $ redirect undefined

redirectExample2
  = respond
  $ redirect undefined

{- Reject -}

rejectExample1
  = respond
  $ reject

rejectExample2
  = respond
  $ (reason .~ Busy $ reject)

{- Pause -}

pauseExample1
  = respond
  . say "I will pause 10 seconds starting now!"
  . (pause <&> Text.XML.Twiml.length .~ 10)
  . say "I just paused 10 seconds"
  $ end

pauseExample2
  = respond
  . (pause <&> Text.XML.Twiml.length .~ 5)
  . say "Hi there."
  $ end

main :: IO ()
main = return ()
