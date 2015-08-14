dialExample3 :: VoiceTwiml
dialExample3 =
  response $ do
    dial' (Left $ Number def "+15558675309") $ def
             & callerId .~ Just "+15551112222"
    end
  where Twiml.Syntax{..} = def
