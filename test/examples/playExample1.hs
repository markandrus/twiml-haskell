playExample1 :: VoiceTwiml
playExample1 =
  response $ do
    play (fromJust $ parseURL "https://api.twilio.com/cowbell.mp3") def
    end
  where Twiml.Syntax{..} = def
