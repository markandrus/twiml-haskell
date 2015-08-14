leaveExample1 :: VoiceTwiml
leaveExample1 =
  response $ do
    leave
    end
  where Twiml.Syntax{..} = def
