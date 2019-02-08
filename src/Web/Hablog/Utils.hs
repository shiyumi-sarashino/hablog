{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Utils where

import Control.Arrow ((&&&))
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import qualified CMarkGFM as GFM
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Internal as BI

hd :: [a] -> Maybe a
hd [] = Nothing
hd (x:_) = Just x

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:xs) n
  | n > 0 = xs `at` (n-1)
  | n == 0 = Just x
  | otherwise = reverse xs `at` (-n)

headerBreaker :: T.Text
headerBreaker = "---"

takeJust :: [Maybe a] -> Maybe a
takeJust [] = Nothing
takeJust (Just x:_) = Just x
takeJust (Nothing:xs) = takeJust xs

convert :: Char -> [String] -> String
convert c str = concatMap (++[c]) (init str) ++ last str

splitBy :: Char -> String -> [String]
splitBy c txt = map reverse $ go [] txt
  where go xs [] = [xs]
        go xs (y:ys)
          | y == c    = xs : go [] ys
          | otherwise = go (y:xs) ys

removeWhitespaces :: String -> String
removeWhitespaces = unwords . words

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

partition :: Char -> T.Text -> (T.Text, T.Text)
partition c = T.takeWhile (/=c) &&& (T.unwords . T.words . T.dropWhile (==c) . T.dropWhile (/=c))


getHeader :: T.Text -> M.Map T.Text T.Text
getHeader = M.fromList . filter ((/=)"" . snd) . map (partition ':') . takeWhile (not . T.isPrefixOf headerBreaker) . T.lines

getContent :: T.Text -> T.Text
getContent = T.unlines . dropWhile (T.isPrefixOf headerBreaker) . dropWhile (not . T.isPrefixOf headerBreaker) . T.lines

createBody :: T.Text -> H.Html
createBody t = BI.Content (BI.PreEscaped $ BI.Text $
                 GFM.commonmarkToHtml
                   [ GFM.optSmart ]
                   [ GFM.extStrikethrough
                   , GFM.extTable
                   , GFM.extAutolink
                   , GFM.extTagfilter
                   ]
                   . T.toStrict $ t) ()
    -- where
        -- preProcess :: T.Text -> T.Text --for two column article, auto page break
        -- preProcess = -- insert '\n---\n' into  threshold behind the number of characters and words
        -- postProcess :: T.Text -> T.Text --for note: syntax, convert '<p>note: *</p>' to '<div class="notes">*</div>'.
        -- postProcess = 



