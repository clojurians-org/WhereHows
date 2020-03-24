#! /usr/bin/env nix-shell
#! nix-shell jdbc-dataset-generator.hs.nix -i runghc

{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes  #-}

{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

import Language.Java (withJVM)
import Language.Java.Inline

main :: IO ()
main = withJVM [] [java| { System.out.println("Hello Java!"); } |]

