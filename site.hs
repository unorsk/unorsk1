--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (listToMaybe)
import Data.Monoid (mappend)
import GHC.Base (empty)
import Hakyll

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "favicon.ico" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- Create lightweight "id" versions for navigation
  match "posts/*" $ version "id" $ do
    compile $ do
      ident <- getUnderlying
      makeItem $ toFilePath ident

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtxWithNav
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
      renderAtom myFeedConfiguration feedCtx posts

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Unorsk",
      feedDescription = "",
      feedAuthorName = "Andrii",
      feedAuthorEmail = "andrii@unorsk.com",
      feedRoot = "https://unorsk.com"
    }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

postCtxWithNav :: Context String
postCtxWithNav =
  field "prevPost" getPrevPostFromIds
    `mappend` field "nextPost" getNextPostFromIds
    `mappend` field "prevTitle" getPrevTitleFromIds
    `mappend` field "nextTitle" getNextTitleFromIds
    `mappend` postCtx

-- Previous = newer post (go back in time to more recent posts)
getPrevPostFromIds :: Item String -> Compiler String
getPrevPostFromIds item = do
  postIds <- loadAll ("posts/*" .&&. hasVersion "id")
  sortedIds <- recentFirst postIds -- [newer, older]
  let currentPath = toFilePath $ itemIdentifier item
      olderFirst = reverse sortedIds -- [older, newer]
  case dropWhile ((/= currentPath) . itemBody) olderFirst of
    (_ : prev : _) -> return $ toUrl $ take (length (itemBody prev) - 9) (itemBody prev) ++ ".html"
    _ -> empty

-- Next = older post (go forward in time to older posts)
getNextPostFromIds :: Item String -> Compiler String
getNextPostFromIds item = do
  postIds <- loadAll ("posts/*" .&&. hasVersion "id")
  sortedIds <- recentFirst postIds -- [newer, older]
  let currentPath = toFilePath $ itemIdentifier item
  case dropWhile ((/= currentPath) . itemBody) sortedIds of
    (_ : next : _) -> return $ toUrl $ take (length (itemBody next) - 9) (itemBody next) ++ ".html"
    _ -> empty

getPrevTitleFromIds :: Item String -> Compiler String
getPrevTitleFromIds item = do
  postIds <- loadAll ("posts/*" .&&. hasVersion "id")
  sortedIds <- recentFirst postIds -- [newer, older]
  let currentPath = toFilePath $ itemIdentifier item
      olderFirst = reverse sortedIds -- [older, newer]
  case dropWhile ((/= currentPath) . itemBody) olderFirst of
    (_ : prev : _) -> do
      let prevIdent = fromFilePath $ itemBody prev
      metadata <- getMetadata prevIdent
      return $ maybe "Previous Post" id $ lookupString "title" metadata
    _ -> empty

getNextTitleFromIds :: Item String -> Compiler String
getNextTitleFromIds item = do
  postIds <- loadAll ("posts/*" .&&. hasVersion "id")
  sortedIds <- recentFirst postIds -- [newer, older]
  let currentPath = toFilePath $ itemIdentifier item
  case dropWhile ((/= currentPath) . itemBody) sortedIds of
    (_ : next : _) -> do
      let nextIdent = fromFilePath $ itemBody next
      metadata <- getMetadata nextIdent
      return $ maybe "Next Post" id $ lookupString "title" metadata
    _ -> empty
