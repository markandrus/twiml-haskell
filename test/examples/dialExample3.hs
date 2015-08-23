dialExample3 :: VoiceTwiml
dialExample3 =
  response $ do
    dial' (Left . dialNoun $ number "+15558675309" def) $ def
             & callerId .~ Just "+15551112222"
    end
  where Twiml.Syntax{..} = def
