{-# LANGUAGE OverloadedStrings #-}

module Lib (getNews) where

import Data.Dates (DateTime(DateTime))
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.HTML.Scalpel
import Text.Read (readMaybe)
import Lucid.Base
import Lucid.Html5

type Title = T.Text
type Link = T.Text

data News
    = TextNews DateTime Link Title
    deriving (Eq, Show, Ord)

formatDate :: DateTime -> T.Text
formatDate (DateTime year month day _ _ _) = T.pack $ sday ++ "." ++ smonth ++ "." ++ syear ++ " "
  where sday = show day
        smonth = show month
        syear = show year

newsToHtml :: News -> Html ()
newsToHtml (TextNews date link title) = a_ [href_ link] $ toHtml (T.append (formatDate date) title)

renderNews :: [[News]] -> [Html ()]
renderNews news = map newsToHtml (take 10 orderedNews)
  where orderedNews = reverse $ sort $ concat news


makeSouthernNews :: Title -> Link -> Maybe News
makeSouthernNews title link = do
    day <- readMaybe $ take 2 stitle
    month <- readMaybe $ take 2 $ drop 3 stitle
    year <- readMaybe $ take 4 $ drop 6 stitle
    return $ TextNews (DateTime year month day 0 0 0) link $ T.pack $ drop 11 stitle
            where stitle = T.unpack title

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
            return $ makeSouthernNews (T.pack title ) (T.pack (southBaseUrl ++ link))

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
            return $ makeNorthernNews timestamp (T.pack title) (T.pack link)

justifyNews :: Maybe [Maybe News] -> [News]
justifyNews = catMaybes . fromMaybe [] 

getNewsHtml :: IO [Html ()]
getNewsHtml = do
      nnews <- northernNews
      snews <- southernNews
      return $ renderNews $ map justifyNews [nnews, snews]

getNews :: IO (Html ())
getNews = do
    news <- getNewsHtml
    return $ doctypehtml_ $ body_ $ mapM_ div_ news
         
--getNews = do
--    nnews <- northernNews
--    snews <- southernNews
--    let news = LT.unpack $ renderNews $ map justifyNews [nnews, snews]
--    return $ "<html><head></head><body><div>" ++ news ++ "</div></body></html>"
