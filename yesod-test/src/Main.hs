{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import           Yesod
import           Control.Monad.Trans
import           Data.Time
import           Data.Text              as T

data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
/                           HomeR       GET
/time                       TimeR       GET
/add/#Integer/#Integer      AddNumR     GET
/add-text/#Text/#Text       AddTextR    GET
|]

-- Yesod was not able to overload the /add/... route based on types.

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<h1>Hello World!|]

getTimeR :: Handler Html
getTimeR = do
        t <- lift $ getCurrentTime
        let time = show t
         in defaultLayout [whamlet|#{time}|]

getAddNumR :: Integer -> Integer -> Handler Html
getAddNumR n1 n2 = defaultLayout [whamlet|#{n1 + n2}|]

getAddTextR :: Text -> Text -> Handler Html
getAddTextR w1 w2 = let w3 = T.concat [w1, w2]
                     in defaultLayout [whamlet|#{w3}|]

main :: IO ()
main = warp 3000 App
