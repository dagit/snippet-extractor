module Multisnippet where

{- @snippet-start Multisnippet1.snip -}
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
{- @snippet-end -}


-- @snippet-start Multisnippet2.snip -}
ones = 1 : ones
-- @snippet-end

main = return ()
