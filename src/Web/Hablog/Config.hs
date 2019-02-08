-- | Configuration for Hablog

{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Config where

import Data.Text.Lazy (Text)

-- | Data type to set the theme for your Hablog blog
data Theme = Theme
  { bgTheme   :: FilePath -- ^ General theme for hablog. a file path for a css file
  , codeTheme :: FilePath -- ^ Theme for code. a file path for a highlight.js css file
  }
  deriving (Show, Read)

-- | Configuration for Hablog
data Config = Config
  { blogTitle  :: Text
  , blogTagLine  :: Text
  , blogTheme  :: Theme
  , blogDomain :: Text
  }
  deriving (Show, Read)

-- | Requires the needed values for runTLS
data TLSConfig = TLSConfig
  { blogTLSPort :: Int
  , blogCert    :: FilePath
  , blogKey     :: FilePath
  }
  deriving (Show, Read)

-- | A default configuration
defaultConfig :: Config
defaultConfig = Config
  { blogTitle = defaultTitle
  , blogTagLine = defaultTagLine
  , blogTheme = snd defaultTheme
  , blogDomain = defaultDomain
  }

-- | "Hablog"
defaultTitle :: Text
defaultTitle = "Hablog-prida"

defaultTagLine :: Text
defaultTagLine = "fork of Hablog"

defaultDomain :: Text
defaultDomain = "localhost"

-- | The default HTTP port is 80
defaultPort :: Int
defaultPort = 80

-- | The default HTTPS port is 443
defaultTLSPort :: Int
defaultTLSPort = 443

-- | The default is the dark theme
defaultTheme :: (String, Theme)
defaultTheme = ("prida", pridaTheme)

darkTheme :: Theme
darkTheme = Theme "/static/css/dark.css" "/static/highlight/styles/hybrid.css"

lightTheme :: Theme
lightTheme  = Theme "/static/css/light.css" "/static/highlight/styles/docco.css"

pridaTheme :: Theme
pridaTheme  = Theme "/static/css/prida.css" "/static/highlight/styles/docco.css"

themes :: [(String, Theme)]
themes =
  [("dark",  darkTheme)
  ,("light", lightTheme)
  ,("prida", pridaTheme)
  ]
