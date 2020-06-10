module Main where

import Ivory.Language
import Iv

-- puts :: Def ('[IString] :-> Sint32)
-- puts  = importProc "puts" "stdio.h"
--
-- mainProc :: Def ('[] :-> ())
-- mainProc  = proc "main" $ body $ do
--   call_ puts "hello, world\n"
--   retVoid

-- mainModule :: Module

main :: IO ()
main = do
  -- runCompiler[mainProc]
  print "YO"
  return ()
