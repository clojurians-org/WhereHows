#! /usr/bin/env nix-shell
#! nix-shell dataset-jdbc-generator.hs.nix -i "runghc --ghc-arg=-fobject-code"

{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import System.Environment (lookupEnv)
import qualified Language.Haskell.TH.Syntax as TH
import Data.String (fromString)


import Control.Concurrent (runInBoundThread)
import Language.Java (withJVM)
import Language.Java.Inline

main :: IO ()
main = do
  let
    jvmArgs = case $(TH.lift =<< TH.runIO (lookupEnv "CLASSPATH")) of
      Nothing -> []
      Just cp -> [ fromString ("-Djava.class.path=" ++ cp) ]

  runInBoundThread $ withJVM jvmArgs [java| {
    try {
      Class.forName("com.mysql.jdbc.Driver") ;
    } catch (ClassNotFoundException e) {
      e.printStackTrace() ;
      System.exit(1) ;
    }

    try {
      final String dbUrl = "jdbc:mysql://localhost:3306/datahub" ;
      final String user = "datahub" ;
      final String password = "datahub" ;
      java.sql.Connection con = java.sql.DriverManager.getConnection(dbUrl, user, password) ;
      String sql = "" ;
      con.close() ;
    } catch (java.sql.SQLException e) {
      e.printStackTrace() ;
      System.exit(1) ;
    }
    System.out.println("Hello Java!") ;
  } |]
 
