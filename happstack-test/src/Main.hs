
import           Control.Monad
import           Control.Monad.Trans
import           Happstack.Server
import           Data.Time

main :: IO ()
main = simpleHTTP nullConf $ msum
        [ dir "time" $ getTime
        , dir "add" $ path $ \n1 -> path $ \n2 -> getAddNum n1 n2
        , dir "add" $ path $ \w1 -> path $ \w2 -> getAddText w1 w2
        , dir "search" $ getSearch
        , getHome
        ]

getHome :: ServerPartT IO Response
getHome = ok (toResponse "<h1>Hello World!</h1>")

getTime :: ServerPartT IO Response
getTime = do
        t <- lift $ getCurrentTime
        ok (toResponse (show t))

getAddNum :: Integer -> Integer -> ServerPartT IO Response
getAddNum n1 n2 = ok (toResponse (show (n1 + n2)))

getAddText :: String -> String -> ServerPartT IO Response
getAddText w1 w2 = ok (toResponse (w1 ++ w2))

getSearch :: ServerPartT IO Response
getSearch = do
        contact <- queryString $ look "contactId"
        company <- queryString $ look "companyId"
        ok (toResponse (
              "<ul>"
           ++ "<li>Contact ID: " ++ contact ++ "</li>"
           ++ "<li>Company ID: " ++ company ++ "</li>"
           ++ "</ul>"
           ))
