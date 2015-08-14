smsExample2 :: VoiceTwiml
smsExample2 =
  response $ do
    say "Our store is located at 123 Easy St." def
    sms "Store Location: 123 Easy St." $ def
            & action .~ parseURL "/smsHandler.php"
            & method .~ Just POST
    end
  where Twiml.Syntax{..} = def
