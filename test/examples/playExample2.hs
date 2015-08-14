playExample2 :: VoiceTwiml
playExample2 =
  response $ do
    play' Nothing $ def & digits .~ Just [W, W, W, W, D3]
    end
  where Twiml.Syntax{..} = def
