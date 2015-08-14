recordExample2 :: VoiceTwiml
recordExample2 =
  response $ do
    say "Please leave a message at the beep. Press the star key when finished." def
    record $ def & action      .~ parseURL "http://foo.edu/handleRecording.php"
                 & method      .~ Just GET
                 & maxLength   .~ Just 20
                 & finishOnKey .~ Just KStar
    say "I did not receive a recording" def
    end
  where Twiml.Syntax{..} = def
