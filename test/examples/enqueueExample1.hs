enqueueExample1 :: VoiceTwiml
enqueueExample1 =
  response $ do
    enqueue "support" $ def & waitURL .~ parseURL "wait-music.xml"
    end
  where Twiml.Syntax{..} = def
