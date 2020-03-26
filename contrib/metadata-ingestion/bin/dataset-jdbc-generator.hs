#! /usr/bin/env nix-shell
#! nix-shell dataset-jdbc-generator.hs.nix -i "runghc --ghc-arg=-fobject-code"

{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}


{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}


import System.Environment (lookupEnv)
import qualified Language.Haskell.TH.Syntax as TH

import Control.Concurrent (runInBoundThread)
import Language.Java (J, withJVM, reify, reflect, JType(..))
import Language.Java.Inline (imports, java)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String.Conversions (cs)
import Text.InterpolatedString.Perl6 (q)

import Prelude hiding ((>>=), (>>))

imports "java.util.*"
imports "java.sql.*"


datasetOracleSql :: String
datasetOracleSql = [q|
    select
      c.OWNER || '.' || c.TABLE_NAME as schema_name
    , t.COMMENTS as schema_description
    , c.COLUMN_NAME as field_path
    , c.DATA_TYPE as native_data_type 
    , m.COMMENTS as description
    from ALL_TAB_COLUMNS c
      left join ALL_TAB_COMMENTS t
        on c.OWNER = t.OWNER
        and c.TABLE_NAME = t.TABLE_NAME
      left join ALL_COL_COMMENTS m
        on c.OWNER = m.OWNER
        and c.TABLE_NAME = m.TABLE_NAME
        and c.COLUMN_NAME = m.COLUMN_NAME
    where NOT REGEXP_LIKE(c.OWNER, 'ANONYMOUS|PUBLIC|SYS|SYSTEM|DBSNMP|MDSYS|CTXSYS|XDB|TSMSYS|ORACLE.*|APEX.*|TEST?*|GG_.*|\\$')
    order by schema_name, c.COLUMN_ID
|]

datasetMysqlSql :: T.Text
datasetMysqlSql = [q|
    select 
      concat(c.TABLE_SCHEMA, '.', c.TABLE_NAME) as schema_name
    , NULL as schema_description
    , c.COLUMN_NAME as field_path
    , c.DATA_TYPE as native_data_type
    , c.COLUMN_COMMENT as description
    from information_schema.columns c
    where table_schema not in ('information_schema') 
    order by schema_name, c.ORDINAL_POSITION
|]

main :: IO ()
main = do
  let
    jvmArgs = case $(TH.lift =<< TH.runIO (lookupEnv "CLASSPATH")) of
      Nothing -> []
      Just cp -> [ cs ("-Djava.class.path=" ++ cp) ]
    dbUrl :: T.Text = "jdbc:mysql://localhost:3306/datahub?useSSL=false"
    dbUser :: T.Text  = "datahub"
    dbPassword :: T.Text = "datahub"
    dbDriver :: T.Text = "com.mysql.jdbc.Driver"
    dbSQL :: T.Text = datasetMysqlSql
  runInBoundThread $ withJVM jvmArgs $ do
    [jDbUrl, jDbUser, jDbPassword, jDbDriver, jDbSQL ] <-
      mapM reflect [dbUrl, dbUser, dbPassword, dbDriver, dbSQL]
    
    result <- [java| {
      try {
        Class.forName($jDbDriver) ;
      } catch (ClassNotFoundException e) {
        e.printStackTrace() ;
        System.exit(1) ;
      }
      
      List<String[]> result = new ArrayList<String[]>() ;
      try (Connection con = DriverManager.getConnection($jDbUrl, $jDbUser, $jDbPassword) ;
           Statement st = con.createStatement(); ) {
        try (ResultSet rs = st.executeQuery($jDbSQL)) {
          while(rs.next()) {
            String[] row  = new String[] {
              Optional.ofNullable(rs.getString("schema_name")).orElse("")
            , Optional.ofNullable(rs.getString("schema_description")).orElse("")
            , Optional.ofNullable(rs.getString("field_path")).orElse("")
            , Optional.ofNullable(rs.getString("native_data_type")).orElse("")
            , Optional.ofNullable(rs.getString("description")).orElse("")
            } ;
            System.out.println(rs.getString("field_path")) ;
            result.add(row) ;
          }
        }
        return result.toArray(new String[0][0]) ;
      } catch (SQLException e) {
        e.printStackTrace() ;
        return null ;
      }
    } |]
    
    xs :: [[T.Text]]  <- reify result
    T.putStrLn (T.unwords (head xs))
    return ()
 
