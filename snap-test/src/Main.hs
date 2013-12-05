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
             , ("/search", getSearchHandler)
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
        handle n1 n2
    where handle (Just n1n) (Just n2n) = let i1 = (read (BS.unpack n1n) :: Integer)
                                             i2 = (read (BS.unpack n2n) :: Integer)
                                          in writeBS $ BS.pack $ show (i1 + i2)
          handle _ _ = writeBS "Error"

getAddTextHandler :: Snap ()
getAddTextHandler = do
        w1 <- getParam "w1"
        w2 <- getParam "w2"
        handle w1 w2
    where handle (Just w1w) (Just w2w) = writeBS $ BS.concat [w1w, w2w]
          handle _ _ = writeBS "Error"

getSearchHandler :: Snap ()
getSearchHandler = do
        contact <- getParam "contactId"
        company <- getParam "companyId"
        handle contact company
    where handle (Just conId) (Just comId) = writeBS $ BS.concat [ "<ul>"
                                                                 , "<li>Contact ID: ", conId, "</li>"
                                                                 , "<li>Company ID: ", comId, "</li>"
                                                                 , "</ul>"
                                                                 ]
          handle _ _ = writeBS "Error"
