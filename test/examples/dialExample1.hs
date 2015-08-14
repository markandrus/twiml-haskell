dialExample1 :: VoiceTwiml
dialExample1 =
  response $ do
    dial "415-123-4567" def
    say "Goodbye" def
    end
  where Twiml.Syntax{..} = def
