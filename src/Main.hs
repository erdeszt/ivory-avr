{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

mainProc :: Def ('[] :-> ())
mainProc  = proc "main" $ body $ do
    retVoid

mainModule :: Module
mainModule = package "firmware" $ do
    incl mainProc

main :: IO ()
main = do
    runCompiler [mainModule] [] initialOpts { outDir = Just "ivory-out" }
    return ()
  where
    options = initialOpts
        { outDir = Just "ivory-out"
        }
