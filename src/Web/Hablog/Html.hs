{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Html where

import Data.String (fromString)
import Data.List (sort, intersperse)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as BI


import Web.Hablog.Config
import Web.Hablog.Types (Anchor(..))
import qualified Web.Hablog.Post as Post
import qualified Web.Hablog.Page as Page

template :: Config -> Bool -> T.Text -> T.Text -> [Page.Page] -> H.Html -> H.Html
template cfg highlight title pageRoute allPages container =
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml (T.concat [blogTitle cfg, " - ", title]))
      H.meta ! A.content "width=device-width, initial-scale=1" ! A.name "viewport"
      H.meta ! A.charset "UTF-8"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/pure/pure-min.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/pure/grids-responsive-min.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.stringValue . bgTheme $ blogTheme cfg)
      if highlight
        then H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.stringValue . codeTheme $ blogTheme cfg)
        else mempty
    H.body $ do
      H.div ! A.class_ "container pure-g" $ do
        sidebar cfg pageRoute allPages
        H.div ! A.class_ "content pure-u-1 pure-u-xl-14-24" $ container
        footer
      if highlight
        then do
          H.script ! A.src "/static/highlight/highlight.pack.js" $ ""
          H.script "hljs.initHighlightingOnLoad();"
        else
          mempty
      H.script ! A.src "/static/js/hablog-prida.js" $ mempty

notFoundPage :: Config -> H.Html
notFoundPage cfg =
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml (T.concat [blogTitle cfg, " - ", "Not Found"]))
      H.meta ! A.content "width=device-width, initial-scale=1" ! A.name "viewport"
      H.meta ! A.charset "UTF-8"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/pure/pure-min.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/pure/grids-responsive-min.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.stringValue . bgTheme $ blogTheme cfg)
    H.body $ do
      H.div ! A.class_ "container pure-g" $ do
        H.div ! A.class_ "notfound" $ do
          H.h1 "Not found"
          H.p "The page you search for is not available."
        footer

sidebar :: Config -> T.Text -> [Page.Page] -> H.Html
sidebar cfg pageRoute allPages = do
  H.div ! A.class_ "sidebar pure-u-1 pure-u-lg-1-4" $ do
    H.div ! A.id "overflow-hidden-parent"  ! A.class_ "header pure-menu" $ do
      H.a ! A.href (H.stringValue "/") ! A.style "text-decoration: none;" $ do
        H.div $ do
          H.div ! A.class_ "brand-title" $ do
            H.img ! A.src "/static/logo.svg" ! A.alt (BI.lazyTextValue (blogTitle cfg)) ! A.width "250"
          H.h2 ! A.class_ "brand-tagline" $ H.toHtml (blogTagLine cfg)
      H.ul ! A.id "overflow-hidden-child" ! A.class_ "nav-list pure-menu-list" $ do
        pagesList pageRoute allPages
      H.div ! A.id "overflow-appear-hamburger" ! A.class_ "hamburger-menu-list" $ do
        H.button ! A.id "hamburger" ! A.class_ "hamburger" ! A.type_ "button" $ do
          H.span ! A.class_ "hamburger-box" $ do
            H.span ! A.class_ "hamburger-inner" $ mempty
        H.ul ! A.id "hamburger-menu" ! A.class_ "hamburger-menu pure-menu-list" $ do
          pagesList pageRoute allPages

footer :: H.Html
footer = do
    H.div ! A.class_ "pure-u-1-4 footer-padding" $ mempty
    H.footer ! A.class_ "footer pure-u-1 pure-u-lg-14-24" $ do
      H.div $ H.a ! A.href "/blog/rss" ! A.class_ "link-color" $ "RSS feed"
      H.span "Powered by "
      H.a ! A.href "https://github.com/shiyumi-sarashino/hablog" ! A.class_ "link-color" $ "Hablog-pride"

errorPage :: Config -> T.Text -> String -> [Page.Page] -> H.Html
errorPage cfg ttl msg pages =
  template cfg False ttl "" pages $ do
    H.h2 $ "Something Went Wrong..."
    H.p $ H.toHtml msg

emptyPage :: H.Html
emptyPage = H.span " "

postsListHtml :: H.Html -> H.Html -> Anchor -> [Post.Post] -> H.Html
postsListHtml auths tgs anchor posts = do
  H.h2 ! A.class_ "pure-menu-heading" $
      case anchor of
         Time t -> do "filter (writtenIn "
                      H.span ! A.class_ "heading-value" $ H.toHtml $ "\"" <> t <> "\""
                      ") Posts"
         Tag tag -> do "filter (hasTag "
                       H.span ! A.class_ "heading-value" $ H.toHtml $ "\"" <> tag <> "\""
                       ") Posts"
         Author auth -> do "filter (writtenBy "
                           H.span ! A.class_ "heading-value" $ H.toHtml $ "\"" <> auth <> "\""
                           ") Posts"
         Raw -> "Posts"
  postsList posts
  H.aside ! A.class_ "aside" $ do
    H.div ! A.class_ "AllAuthorsList pure-u-1 pure-u-xl-1-2" $ do
      H.a ! A.href "/blog/authors" ! A.class_ "list-header" $ "Authors"
      auths
    H.div ! A.class_ "AllTagsList pure-u-1 pure-u-xl-1-2" $ do
      H.a ! A.href "/blog/tags" ! A.class_ "list-header" $ "Tags"
      tgs

postsList :: [Post.Post] -> H.Html
postsList = (H.ul ! A.class_ "pure-menu-list") . mconcat . fmap postsListItem

postsListItem :: Post.Post -> H.Html
postsListItem post = H.li ! A.class_ "pure-menu-item blog-item" $ do
  H.a ! A.class_ "pure-menu-link blog-link" ! A.href (fromString $ T.unpack ("/blog/" `T.append` Post.getPath post)) $ do
    H.toHtml $ Post.title post
  H.div ! A.class_ "post-meta" $ do
    H.span ! A.class_ "link-date" $ do
      H.a ! A.class_ "link-day" ! A.href (BI.lazyTextValue $ "/blog/post/" <> yyyy <> "/" <> mm <> "/" <> dd) $ H.toHtml $ dd <> "/"
      H.a ! A.class_ "link-month" ! A.href (BI.lazyTextValue $ "/blog/post/" <> yyyy <> "/" <> mm) $ H.toHtml $ mm <> "/"
      H.a ! A.class_ "link-year" ! A.href (BI.lazyTextValue $ "/blog/post/" <> yyyy) $ H.toHtml $ yyyy
    H.span $ " "
    authorsIntercalate " , " " and " (fmap authorsListItemAnchor . sort . Post.authors $ post) mempty
    H.span $ ". "
  H.div ! A.class_ "post-meta" $ do
    H.span $ "Tags: ["
    mconcat . intersperse (", ") $ fmap tagsListItemAnchor . sort . Post.tags $ post
    H.span $ "]"
  H.p ! A.class_ "leading-summary" $ H.toHtml . Post.leadingSummary $ post
  where
      yyyy = Post.year $ post
      mm = Post.month $ post
      dd = Post.day $ post


postPage :: Config -> Post.Post -> [Page.Page] -> H.Html
postPage cfg post pages = template cfg True (Post.title post) "blog" pages $
    H.article $ do
      H.h2 ! A.class_ "postTitle" $ H.toHtml . Post.title $ post
      H.table ! A.class_ "postMeta" $ do
        H.tbody $ do
          H.tr $ do
            H.td $ "author:    "
            H.td $  H.toHtml $ authorsIntercalate " , " " and " (fmap authorsListItemAnchor . sort . Post.authors $ post) mempty
          H.tr $ do
            H.td $ "date:"
            H.td $ H.toHtml . Post.getDate $ post
      H.div ! A.class_ "abstraction-content" $ do
        H.p ! A.class_ "abstraction-title" $ "[abstraction]"
        H.p ! A.class_ "abstraction-body" $ H.toHtml . Post.summary $ post
      H.div ! A.class_ "keys-content" $ do
        H.p ! A.class_ "keys-title" $ "[keywords]"
        H.p ! A.class_ "keys-list" $ (mconcat . intersperse (", ") $ fmap tagsListItemAnchor . sort . Post.tags $ post) <> "."
      H.div ! A.class_ "markdown-body content-2col" $ do
        Post.content post

pagePage :: Config -> Page.Page -> [Page.Page] -> H.Html
pagePage cfg page allPages = template cfg True (Page.getPageName page) (T.pack $ Page.getPageURL page) allPages $ pageContent page

pageContent :: Page.Page -> H.Html
pageContent page = do
  H.article ! A.class_ "markdown-body" $ do
    Page.getPageContent page

pagesList :: T.Text -> ([Page.Page] -> H.Html)
pagesList pageRoute = mconcat . fmap (pagesListItem pageRoute) . sort

pagesListItem :: T.Text -> Page.Page -> H.Html
pagesListItem _ (Page.Links x y z) =
  H.li
    ! A.class_ "nav-item pure-menu-item pure-menu-has-children" $ do
    H.a ! A.href "#" ! A.class_ "pure-menu-link"
      $ H.toHtml $ Page.getPageName $ Page.Links x y z
    H.ul ! A.class_ "pure-menu-children" $ do
      linksLi $ Page.getLinksList $ Page.Links x y z
  where
    linksLi :: M.Map T.Text T.Text -> H.Html
    linksLi linkMap =
      M.foldlWithKey f mempty linkMap
    f :: H.Html -> T.Text -> T.Text -> H.Html
    f a k b = do
      a
      H.li ! A.class_ "nav-item pure-menu-item" $
        H.a ! A.href (BI.lazyTextValue b) ! A.class_ "pure-menu-link" $ H.toHtml k
pagesListItem pageRoute page =
  H.li
    ! A.class_ (BI.stringValue ("nav-item pure-menu-item" ++ (selected (T.unpack pageRoute) (Page.getPageURL page))))
    $ H.a ! A.href (fromString ("/" ++ Page.getPageURL page)) ! A.class_ "pure-menu-link"
      $ H.toHtml (Page.getPageName page)
  where
    selected "" _ = " pure-menu-selected"
    selected route pageName = if pageName == route
                                        then " pure-menu-selected" else ""

tagsList :: [T.Text] -> H.Html
tagsList = H.ul . mconcat . fmap tagsListItem . sort

tagsListItem :: T.Text -> H.Html
tagsListItem tag = H.li $ tagsListItemAnchor tag

tagsListItemAnchor :: T.Text -> H.Html
tagsListItemAnchor tag = H.a ! A.href (fromString $ T.unpack ("/blog/tags/" `T.append` tag)) ! A.class_ "link-color" $ H.toHtml tag

authorsIntercalate :: T.Text -> T.Text -> [H.Html] -> H.Html -> H.Html
authorsIntercalate del lastDel (h:hs) (BI.Empty ()) = authorsIntercalate del lastDel hs h
authorsIntercalate del lastDel (h:[]) ac = authorsIntercalate del lastDel [] $ do
                                          ac
                                          H.span $ H.toHtml $ lastDel
                                          h
authorsIntercalate del lastDel (h:hs) ac = authorsIntercalate del lastDel hs $ do
                                          ac
                                          H.span $ H.toHtml $ del
                                          h
authorsIntercalate _ _ [] ac = ac

authorsList :: [T.Text] -> H.Html
authorsList = H.ul . mconcat . fmap authorsListItem . sort

authorsListItem :: T.Text -> H.Html
authorsListItem author = H.li $ authorsListItemAnchor author

authorsListItemAnchor :: T.Text -> H.Html
authorsListItemAnchor author = H.a ! A.href (fromString $ T.unpack ("/blog/authors/" `T.append` author)) ! A.class_ "link-color" $ H.toHtml author


