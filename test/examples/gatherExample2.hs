gatherExample2 :: VoiceTwiml
gatherExample2 =
  response $ do
    gather (def & action .~ parseURL "/process_gather.php"
                 & method .~ Just GET) $ do
      say "Please enter your account number, followed by the pound sign" def
      end
    say "We didn't receive any input. Goodbye!" def
    end
  where Twiml.Syntax{..} = def
