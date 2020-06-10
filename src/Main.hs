{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

puts :: Def ('[IString] :-> Sint32)
puts  = importProc "puts" "stdio.h"

mainProc :: Def ('[] :-> ())
mainProc  = proc "main" $ body $ do
  call_ puts "hello, world\n"
  retVoid

mainModule :: Module
mainModule = package "test" $ do
  incl puts
  incl mainProc

main :: IO ()
main = do
  runCompiler [mainModule] [] initialOpts { outDir = Nothing }
  return ()
