module TestHelper where

-- HACK: This is a little hideous. Something to never use outside of test code; at least
--       it's not a timebomb waiting to go off like it would be in "real" code.
fromRight :: Show a => Either a c -> c
fromRight e = either (error . show) id e
