#! /usr/bin/env nix-shell
#! nix-shell dataset-jdbc-generator.hs.nix -i "runghc --ghc-arg=-fobject-code"

{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Control.Concurrent (runInBoundThread)
import Language.Java (withJVM)
import Language.Java.Inline

main :: IO ()
main = runInBoundThread $ withJVM [] [java| {
  System.out.println("Hello Java!") ;
} |]
 
