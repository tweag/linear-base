import System.Exit

-- Since this is a test suite of type exitcode-stdio-1.0
-- it succeeds if main exits without an error code.
-- This is just a set of examples, so we always exit with success.

main :: IO ()
main = exitSuccess
