import Distribution.Simple
import System.Process

main
  = defaultMainWithHooks
  $ simpleUserHooks {
      preBuild = \_ _ -> do
        system "make"
        return (Nothing, [])
    }
