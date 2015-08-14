rejectExample2 :: VoiceTwiml
rejectExample2 =
  response $ do
    reject $ def & reason .~ Just Busy
    end
  where Twiml.Syntax{..} = def
