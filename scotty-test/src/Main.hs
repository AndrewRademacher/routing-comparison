{-# LANGUAGE OverloadedStrings #-}

import           Web.Scotty
import           Control.Monad.Trans
import           Data.Time
import           Data.Text.Lazy         as LT

main :: IO ()
main = scotty 3000 $ do
    get "/" getHome
    get "/time" getTime
    get "/add/:n1/:n2" getAddNum
    get "/add/:w1/:w2" getAddText

getHome :: ActionM ()
getHome = html "<h1>Hello World!</h1>"

getTime :: ActionM ()
getTime = do
        t <- lift $ getCurrentTime
        html $ LT.pack $ show t

getAddNum :: ActionM ()
getAddNum = do
        n1 <- param "n1"
        n2 <- param "n2"
        html $ LT.pack $ show ((n1::Integer) + (n2::Integer))

getAddText :: ActionM ()
getAddText = do
        w1 <- param "w1"
        w2 <- param "w2"
        html $ LT.concat [(w1::Text), (w2::Text)]
