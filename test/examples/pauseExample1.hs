pauseExample1 :: VoiceTwiml
pauseExample1 =
  response $ do
    say "I will pause 10 seconds starting now!" def
    pause $ def & duration .~ Just 10
    say "I just paused 10 seconds" def
    end
  where Twiml.Syntax{..} = def
