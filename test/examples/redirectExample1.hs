redirectExample1 :: VoiceTwiml
redirectExample1 =
  response $ do
    dial "415-123-4567" def
    redirect (fromJust $ parseURL "http://www.foo.com/nextInstructions") def
    end
  where Twiml.Syntax{..} = def
