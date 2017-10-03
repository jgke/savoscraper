{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Dates (DateTime(DateTime))
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Template
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.HTML.Scalpel
import Text.Read (readMaybe)

type Title = String
type Link = String

data News
    = TextNews DateTime Link Title
    deriving (Eq, Show, Ord)

newsHtmlTemplate :: Template
newsHtmlTemplate = template "<a href=\"${link}\">${date} ${title}</a><br />\n"

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

formatDate :: DateTime -> String
formatDate (DateTime year month day _ _ _) = sday ++ "." ++ smonth ++ "." ++ syear
  where sday = show day
        smonth = show month
        syear = show year

unmarshal :: News -> [(T.Text, T.Text)]
unmarshal (TextNews date link title) = [("date", tdate), ("link", tlink), ("title", ttitle)]
  where tdate = T.pack $ formatDate date
        tlink = T.pack link
        ttitle = T.pack title

newsToContext :: News -> Context
newsToContext news = context $ unmarshal news

showNews :: News -> LT.Text
showNews news = render newsHtmlTemplate (newsToContext news)
-- printNews news = putStrLn $ trace (show $ unmarshal news) $ show news

renderNews :: [[News]] -> LT.Text
renderNews news = LT.concat $ map showNews (take 10 orderedNews)
  where orderedNews = reverse $ sort $ concat news


makeSouthernNews :: Title -> Link -> Maybe News
makeSouthernNews title link = do
        day <- readMaybe $ take 2 title
        month <- readMaybe $ take 2 $ drop 3 title
        year <- readMaybe $ take 4 $ drop 6 title
        return $ TextNews (DateTime year month day 0 0 0) link $ drop 11 title

southBaseUrl :: String
southBaseUrl = "http://www.esavo.fi"

southUrl :: String
southUrl = southBaseUrl ++ "/tiedotteet"

southernNews :: IO (Maybe [Maybe News])
southernNews = scrapeURL southUrl allNews
    where
        allNews :: Scraper String [Maybe News]
        allNews = chroots ("div" @: [hasClass "newslist-item"]) news

        news :: Scraper String (Maybe News)
        news = do
            title <- text $ "a"
            link <- attr "href" $ "a"
            return $ makeSouthernNews title (southBaseUrl ++ link)

makeNorthernNews :: String -> Title -> Link -> Maybe News
makeNorthernNews date title link = do
        day <- readMaybe $ take 2 $ drop 6 date
        month <- readMaybe $ take 2 $ drop 4 date
        year <- readMaybe $ take 4 date
        return $ TextNews (DateTime year month day 0 0 0) link title

northernNews :: IO (Maybe [Maybe News])
northernNews = scrapeURL "https://www.pohjois-savo.fi/tietopalvelut/uutispoyta.html" allNews
    where
        allNews :: Scraper String [Maybe News]
        allNews = chroots ("div" @: [hasClass "col-s-6"]) news

        news :: Scraper String (Maybe News)
        news = do
            timestamp <- text $ "span" @: [hasClass "timestamp"]
            title <- text $ ("h3" @: [hasClass "title"]) // "a"
            link <- attr "href" $ ("h3" @: [hasClass "title"]) // "a"
            return $ makeNorthernNews timestamp title link

justifyNews :: Maybe [Maybe News] -> [News]
justifyNews = catMaybes . fromMaybe [] 

getNews :: IO String
getNews = do
    nnews <- northernNews
    snews <- southernNews
    let news = LT.unpack $ renderNews $ map justifyNews [nnews, snews]
    return $ "<html><head></head><body><div>" ++ news ++ "</div></body></html>"

main :: IO ()
main = do
    news <- getNews
    putStrLn news
