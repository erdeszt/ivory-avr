module Main where

import Ivory.Compile.C.CmdlineFrontend
    ( runCompiler, initialOpts, Opts(outDir) )
import Ivory.HW ( hw_artifacts, hw_moduledef )
import Ivory.Language ( incl, package, Module )
import GHC.TypeNats ()

import System.Directory ( getCurrentDirectory )

import Blink ( blinkMain )
import Delay ( delayInit, delayMS )

mainModule :: Module
mainModule = package "firmware" $ do
    hw_moduledef
    incl delayInit
    incl delayMS
    incl blinkMain

main :: IO ()
main = do
  runCompiler [mainModule] hw_artifacts options
  cwd <- getCurrentDirectory
  putStrLn cwd
  return ()
  where
    options = initialOpts
        { outDir = Just "build/ivory"
        }

