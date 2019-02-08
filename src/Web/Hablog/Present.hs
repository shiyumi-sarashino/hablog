{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Present where

import           Web.Scotty.Trans
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (catMaybes, Maybe(..))
import           Data.Either (rights)
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))

import qualified System.Directory as DIR (getDirectoryContents)
import           System.IO.Error (catchIOError)
import qualified Text.RSS as RSS

import Web.Hablog.Html
import Web.Hablog.Types
import Web.Hablog.Config
import qualified Web.Hablog.Post as Post
import qualified Web.Hablog.Page  as Page
import Network.URI (URI)


presentBlog :: HablogAction ()
presentBlog = do
  allPosts <- liftIO getAllPosts
  allPages <- liftIO getAllPages
  tgs <- liftIO getTagList
  auths <- liftIO getAuthorsList
  cfg <- getCfg
  html $ HR.renderHtml $ template cfg False "Blog" "blog" allPages $
    postsListHtml auths tgs Raw allPosts

presentRSS :: URI -> HablogAction ()
presentRSS domain = do
  cfg <- getCfg
  allPosts <- liftIO getAllPosts
  let mime = "application/rss+xml"
  setHeader "content-type" mime
  raw
    . BSLC.pack
    . RSS.showXML
    . RSS.rssToXML
    . RSS.RSS (T.unpack $ blogTitle cfg) domain "" []
    . map (Post.toRSS $ blogDomain cfg)
    $ allPosts

showPostsWhere :: (Post.Post -> Bool) -> T.Text -> HablogAction ()
showPostsWhere test eq = do
  cfg <- getCfg
  tgs <- liftIO getTagList
  auths <- liftIO getAuthorsList
  allPosts <- liftIO getAllPosts
  allPages <- liftIO getAllPages
  html $ HR.renderHtml $ template cfg False "Posts" "blog" allPages $
    postsListHtml auths tgs (Time eq) $ filter test allPosts

presentPage :: T.Text -> HablogAction ()
presentPage route = do
  pages <- liftIO getAllPages
  showOrNotFound presentPage' . filter (((==) $ T.unpack $ T.toLower route) . (\p -> case p of
                                                                                       Page.Page _ _ _ _ -> Page.getPageURL p
                                                                                       Page.Links _ _ _ -> "")) $ pages

presentPage' :: Config -> Page.Page -> [Page.Page] -> H.Html
presentPage' cfg page pages = do
  template cfg False (Page.getPageName page) (T.pack $ Page.getPageURL page) pages $ do
    pageContent page



getAllPages :: IO [Page.Page]
getAllPages = getAllFromDir Page.toPage "_pages"

getAllPosts :: IO [Post.Post]
getAllPosts = getAllFromDir Post.toPost "_posts"

getAllFromDir :: Ord a => (T.Text -> Maybe a) -> FilePath -> IO [a]
getAllFromDir parse dir = do
  posts <- fmap (L.delete ".." . L.delete ".") (DIR.getDirectoryContents dir `catchIOError` (\_ -> pure []))
  contents <- rights <$> mapM ((\x -> (T.decodeUtf8' <$> BSL.readFile x) `catchIOError` const (pure $ Left undefined)) . ((dir++"/")++)) posts
  pure . L.sortBy (flip compare) . catMaybes $ fmap parse (reverse contents)

presentPost :: T.Text -> HablogAction ()
presentPost title = do
  posts <- liftIO getAllPosts
  showOrNotFound postPage $ filter ((== title) . path) posts
  where path p = T.intercalate "/" ([Post.year, Post.month, Post.day, Post.route] <*> [p])

showOrNotFound :: (Config -> a -> [Page.Page] -> H.Html) -> [a] -> HablogAction ()
showOrNotFound showP result = do
  cfg <- getCfg
  pages <- liftIO getAllPages
  case result of
    (p:_) -> html $ HR.renderHtml $ showP cfg p pages
    []    -> html $ HR.renderHtml $ errorPage cfg "Hablog - 404: not found" "Could not find the page you were looking for." pages

presentTags :: HablogAction ()
presentTags = do
  cfg <- getCfg
  allPages <- liftIO getAllPages
  tags <- liftIO getTagList
  html . HR.renderHtml $ template cfg False "Posts Tags" "blog" allPages $
    H.div ! A.class_ "AllTagsList pure-u-1" $ do
      H.h2 ! A.class_ "pure-menu-heading" $ "Tags"
      tags

getTagList :: IO H.Html
getTagList = pure . tagsList . getAllTags =<< getAllPosts

getPageList :: T.Text -> [Page.Page] -> H.Html
getPageList pageRoute pages = H.ul $ pagesList pageRoute pages


getAuthorsList :: IO H.Html
getAuthorsList = pure . authorsList . getAllAuthors =<< getAllPosts

presentTag :: T.Text -> HablogAction ()
presentTag tag = do
  cfg <- getCfg
  tgs <- liftIO getTagList
  auths <- liftIO getAuthorsList
  allPages <- liftIO getAllPages
  posts <- liftIO getAllPosts
  html . HR.renderHtml . template cfg False tag "blog" allPages $ postsListHtml auths tgs (Tag tag) $ filter (hasTag tag) posts

presentAuthors :: HablogAction ()
presentAuthors = do
  cfg <- getCfg
  allPages <- liftIO getAllPages
  authors <- liftIO getAuthorsList
  html . HR.renderHtml $ template cfg False "Posts Authors" "blog" allPages $
    H.div ! A.class_ "AllAuthorsList pure-u-1" $ do
      H.h2 ! A.class_ "pure-menu-heading" $ "Authors"
      authors

presentAuthor :: T.Text -> HablogAction ()
presentAuthor auth = do
  cfg <- getCfg
  tgs <- liftIO getTagList
  auths <- liftIO getAuthorsList
  allPages <- liftIO getAllPages
  posts <- liftIO getAllPosts
  html . HR.renderHtml . template cfg False auth "blog" allPages . postsListHtml auths tgs (Author auth) $ filter (hasAuthor auth) posts

getPageFromFile :: T.Text -> IO (Maybe Page.Page)
getPageFromFile title = do
  let path = T.unpack $ mconcat ["_pages/", title]
  getFromFile Page.toPage path

getPostFromFile :: T.Text -> T.Text -> IO (Maybe Post.Post)
getPostFromFile date title = do
  let postPath = T.unpack $ mconcat ["_posts/", date, "-", title, ".md"]
  getFromFile Post.toPost postPath

getFromFile :: (T.Text -> Maybe a) -> String -> IO (Maybe a)
getFromFile constructor path = do
  fileContent <- (T.decodeUtf8' <$> BSL.readFile path) `catchIOError` const (pure $ Left undefined)
  let cont = case fileContent of
              Left _  -> Nothing
              Right x -> Just x
  let content = constructor =<< cont
  pure content

getAllTags :: [Post.Post] -> [T.Text]
getAllTags = getAll Post.tags

hasTag :: T.Text -> Post.Post -> Bool
hasTag tag = ([]/=) . filter (==tag) . Post.tags

getAllAuthors :: [Post.Post] -> [T.Text]
getAllAuthors = getAll Post.authors

getAll :: (Post.Post -> [T.Text]) -> [Post.Post] -> [T.Text]
getAll f  = L.sort . map (T.unwords . T.words . head) . L.group . L.sort . concatMap f

hasAuthor :: T.Text -> Post.Post -> Bool
hasAuthor auth myPost = auth `elem` Post.authors myPost

