{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans
import           Snap.Core
import           Snap.Http.Server
import           Data.Time
import           Data.ByteString.Char8          as BS

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("time", getTimeHandler)
             , ("add", pathArg $ \a -> pathArg $ getAddNumHandler a)
             , ("add", pathArg $ \a -> pathArg $ getAddTextHandler a)
             , ("search", getSearchHandler)
             , ("", ifTop getHomeHandler)
             ]

getHomeHandler :: Snap ()
getHomeHandler = writeBS "<h1>Hello World!</h1>"

getTimeHandler :: Snap ()
getTimeHandler = writeBS . BS.pack . show =<< liftIO getCurrentTime

getAddNumHandler :: Integer -> Integer -> Snap ()
getAddNumHandler n1 n2 = writeBS $ BS.pack $ show (n1 + n2)

getAddTextHandler :: ByteString -> ByteString -> Snap ()
getAddTextHandler w1 w2 = writeBS $ BS.concat [w1, w2]

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

