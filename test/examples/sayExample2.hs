sayExample2 :: VoiceTwiml
sayExample2 =
  response $ do
    say "Bom dia." $ def & voice .~ Just (Alice $ Just PtBR)
                         & loop  .~ Just 2
    end
  where Twiml.Syntax{..} = def
