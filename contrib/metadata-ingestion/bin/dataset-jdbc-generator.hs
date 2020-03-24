#! /usr/bin/env nix-shell
#! nix-shell dataset-jdbc-generator.hs.nix -i runghc

{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Language.Java (withJVM)
import Language.Java.Inline

main :: IO ()
main = withJVM [] [java| {
  System.out.println("Hello Java!") ;
} |]
 
