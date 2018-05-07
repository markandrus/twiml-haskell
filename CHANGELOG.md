0.2.1.0 (May 7, 2018)
=====================

New Features
------------

* Added `messagingResponse` function for building MessagingTwiml.
* Added `voiceResponse` function for building VoiceTwiml. The existing
  `response` is the same as `voiceResponse`.

Bug Fixes
---------

* Removed a Makefile that was used to generate examples and replaced it with
  [doctest](https://hackage.haskell.org/package/doctest). (#9, #10)

0.2.0.1 (August 13, 2017)
=========================

I updated the library to build with newer GHC versions.
