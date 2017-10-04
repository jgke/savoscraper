{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Lucid.Base (Html)
import Servant.HTML.Lucid

import Lib

type NewsApi = Get '[HTML] (Html ())

fetchNews :: Handler (Html ())
fetchNews = liftIO $ getNews

server :: Server NewsApi
server = fetchNews

rootApi :: Proxy NewsApi
rootApi = Proxy

app :: Application
app = serve rootApi server

execute :: IO ()
execute = run 8081 app

main :: IO ()
main = execute
    -- news <- getNews
    -- putStrLn news
