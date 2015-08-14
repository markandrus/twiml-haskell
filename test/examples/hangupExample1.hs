hangupExample1 :: VoiceTwiml
hangupExample1 =
  response $ do
    hangup
    end
  where Twiml.Syntax{..} = def
