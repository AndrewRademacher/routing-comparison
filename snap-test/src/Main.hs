{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans
import           Control.Applicative
import           Snap.Core
import           Snap.Http.Server
import           Data.Time
import           Data.ByteString.Char8          as BS

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = ifTop (getHomeHandler) <|>
       route [ ("time", getTimeHandler)
             , ("/add/:n1/:n2", getAddNumHandler)
             , ("/add-text/:w1/:w2", getAddTextHandler)
             ]

-- No support for overloading that same route by type.

getHomeHandler :: Snap ()
getHomeHandler = writeBS "<h1>Hello World!</h1>"

getTimeHandler :: Snap ()
getTimeHandler = do
        t <- liftIO $ getCurrentTime
        writeBS $ BS.pack $ show t

getAddNumHandler :: Snap ()
getAddNumHandler = do
        n1 <- getParam "n1"
        n2 <- getParam "n2"
        case n1 of
            Just n1s -> case n2 of
                            Just n2s -> let n1n = (read (BS.unpack n1s) :: Integer)
                                            n2n = (read (BS.unpack n2s) :: Integer)
                                         in writeBS $ BS.pack $ show (n1n + n2n)
                            Nothing  -> writeBS "Error"
            Nothing  -> writeBS "Error"

getAddTextHandler :: Snap ()
getAddTextHandler = do
        w1 <- getParam "w1"
        w2 <- getParam "w2"
        case w1 of
            Just w1s -> case w2 of
                            Just w2s -> writeBS $ BS.concat [w1s, w2s]
                            Nothing  -> writeBS "Error"
            Nothing  -> writeBS "Error"
