{-# LANGUAGE DoAndIfThenElse, OverloadedStrings #-}

module Main where


import Database.Persist.Postgresql    ( createPostgresqlPool )

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import qualified Data.List                  as DL
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.LibPQ  as PQ
import qualified Database.PostgreSQL.Simple as PS

import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Logger

import Control.Monad.IO.Class

dbtest :: T.Text -> T.Text -> IO ()
dbtest username password = do
    print ("test"::String)  
    conBS1 <- return $ B.intercalate " " [ helper "host"   hostname
                                  , helper "port"   port
                                  , helper "dbname" dbname
                                  , helper "user"     $ encodeUtf8 username
                                  , helper "password" $ encodeUtf8 password
                                  ]  
    print conBS1
    print ("test1"::String)  
    pqp1 <- do 
       liftIO $
         do 
            PQ.connectdb conBS1 >>= PQ.status >>= print
            return ()
       return ()
    liftIO $ print ("test2"::String)  
    simple1 <- return $ PS.connectPostgreSQL conBS1
    liftIO $ print ("test3"::String)  
    pool1 <- (runStdoutLoggingT $ createPostgresqlPool conBS1 20)
    -- pool1 <- 
    liftIO $ print ("test4"::String)  
    -- liftIO $ PQ.db pqp1 >>= (liftIO . print)
    where
      searchPath            = T.empty
      w64cardinality        = 4
      applicationNameMaxLen = 63
      hostname              = "localhost"
      port                  = "5432"
      dbname                = "converter"
      helper k v            = B.concat [k, "='", v, "'"]


main :: IO ()
main = do
  putStrLn "hello world"
  dbtest "admin" "admin"
