{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Page where

import           Control.Arrow  ((&&&))
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import qualified Text.Blaze.Html5 as H

import Web.Hablog.Utils

data Page
    = Page
    { getPageURL      :: FilePath
    , getPageName     :: T.Text
    , getPagePriority :: Int
    , getPageContent  :: H.Html
    }
    | Links
    { getPageName     :: T.Text
    , getPagePriority :: Int
    , getLinksList  :: M.Map T.Text T.Text
    }


toPage :: T.Text -> Maybe Page
toPage fileContent =
  case (M.lookup "links" header) of
    (Just "True") ->
      Links <$> M.lookup "title" header
            <*> maybeDecimal (M.lookup "priority" header)
            <*> (Just $ M.fromList . filter ((/=)"" . snd) . map (partition ':') $ T.lines content)
    _ ->
      Page <$> fmap T.unpack (M.lookup "route" header)
           <*> M.lookup "title" header
           <*> maybeDecimal (M.lookup "priority" header)
           <*> pure (createBody content)
    where (header, content) = (getHeader &&& getContent) fileContent
          maybeDecimal :: Maybe T.Text -> Maybe Int
          maybeDecimal (Just priority) = _maybeDecimal . TR.decimal $ priority
          maybeDecimal Nothing = Nothing
          _maybeDecimal :: Either String (Int, T.Text) -> Maybe Int
          _maybeDecimal (Right (a, _)) = Just a
          _maybeDecimal (Left _) = Nothing

instance Show Page where
  show = getPageURL

instance Eq Page where
  (==) p1 p2 = getPageURL p1 == getPageURL p2

instance Ord Page where
  compare p1 p2
    | getPagePriority p1 < getPagePriority p2 = LT
    | otherwise = GT

