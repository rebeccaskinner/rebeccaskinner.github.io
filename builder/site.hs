--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid              (mappend, mconcat)
import           Hakyll
import           Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import           Text.Pandoc.Options      (ReaderOptions (..),
                                           WriterOptions (..))
import           Data.List

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown","resume.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= saveSnapshot "postPreTemplate"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let
              indexCtx =
                mconcat [ listField "posts" postCtx (return posts)
                        , defaultContext
                        ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    mconcat
      [ dateField "date" "%B %e, %Y"
      , field "readingtime" readingTime
      , field "previewBody" previewBody
      , field "fadePreviewBody" previewFade
      , field "wordCount" wordCount
      , defaultContext
      ]

previewLineCount = 500
previewFadeCount = 100

previewBody :: Item String -> Compiler String
previewBody Item {..} = do
  rawBody <- loadSnapshotBody itemIdentifier "postPreTemplate"
  pure . unwords . take previewLineCount . words $ rawBody

previewFade :: Item String -> Compiler String
previewFade Item {..} = do
  rawBody <- loadSnapshotBody itemIdentifier "postPreTemplate"
  pure . unwords . take previewFadeCount . drop previewLineCount . words $ rawBody

wordCount :: Item String -> Compiler String
wordCount Item {itemBody} =
  let
    wordCount = length (words itemBody)
  in pure $ show wordCount <> " words"

readingTime :: Item String -> Compiler String
readingTime Item {itemBody} =
  let
    calculatedMinutes = length (words itemBody) `div` 200
    actualMinutes = max 1 calculatedMinutes
  in pure $ case actualMinutes of
              1 -> "1 minute"
              n -> show n <> " minutes"
