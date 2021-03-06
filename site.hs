--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Applicative as A
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), First(..))
import Data.Time (iso8601DateFormat)
import Debug.Trace
import Hakyll
import qualified Data.Set
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.Extensions (Extension(Ext_smart), disableExtension)
import System.FilePath
       (joinPath, splitFileName, splitPath, dropFileName, (-<.>))

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match "favicon.ico" $ do
      route idRoute
      compile copyFileCompiler

    match "google*.html" $ do
      route idRoute
      compile copyFileCompiler

    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["about.markdown"]) $ do
      route $ constRoute "about/index.html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" baseContext >>=
        relativizeUrls >>=
        removeIndexHtml

    match "posts/*/index.*" $ do
      route postRoute
      compile $
        pandocCompilerWith defaultHakyllReaderOptions myHakyllWriterOptions >>=
        loadAndApplyTemplate "templates/post.html" postCtx >>=
        saveSnapshot "feed" >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        relativizeUrls >>=
        removeIndexHtml

    match "drafts/*/index.*" $ do
      route postRoute
      let ctx = noIndex postCtx
      compile $
        pandocCompilerWith defaultHakyllReaderOptions myHakyllWriterOptions >>=
        loadAndApplyTemplate "templates/post.html" ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        relativizeUrls >>=
        removeIndexHtml

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*/index.*"
        let indexCtx = listField "posts" postCtx (return posts) <> baseContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls >>=
          removeIndexHtml

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = bodyField "description" `mappend` postCtx
        posts <-
          fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*/index.*" "feed"
        renderAtom atomConfiguration feedCtx posts


--------------------------------------------------------------------------------
-- |Create a Context with key and value given by the functino f applied to
-- the item metadata. If f returns Nothing an empty field will be created
withMetaContext :: String -> (Metadata -> Maybe String) -> Context String
withMetaContext key f =
  Context $ \k args i ->
    if k == key
      then getField i
      else A.empty
  where
    getField item = do
      meta <- getMetadata (itemIdentifier item)
      case f meta of
        (Just value) -> return (StringField value)
        _ -> A.empty

-- |Create a key in the context, setting its value from metadata.
-- defaultKeys will be searched in the metadata using the first one
-- that is present. The defaultValue is a last alternative.
-- If no matching keys are found in the metadata and defaultValue == Nothing
-- an empty attribute is created
metaDefaultContext :: String -> [String] -> Maybe String -> Context String
metaDefaultContext key defaultKeys defaultValue = withMetaContext key getKey
  where
    getKey :: Metadata -> Maybe String
    getKey meta = foldr1 (A.<|>) options
      where
        options =
          map (`lookupString` meta) (key : defaultKeys) ++ [defaultValue]

defaultDescription :: String
defaultDescription =
  "My personal blog. I write mostly about programming, particularly Haskell and other functional languages"

defaultTitle :: String
defaultTitle = "Sebastian Galkin's Blog"

baseContext :: Context String
baseContext =
  metaDefaultContext
    "meta-description"
    ["description"]
    (Just defaultDescription) <>
  metaDefaultContext "meta-title" ["title"] (Just defaultTitle) <>
  mapContext removeIndexStr (urlField "canonicalUrl") <>
  defaultContext

baseUrl :: String
baseUrl = "https://blog.sebastian-galkin.com"

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  dateField "isoDate" (iso8601DateFormat Nothing) <>
  baseContext

noIndex :: Context a -> Context a
noIndex = (boolField "noindex" (const True) <>)

atomConfiguration :: FeedConfiguration
atomConfiguration =
  FeedConfiguration
  { feedTitle = defaultTitle
  , feedDescription = defaultDescription
  , feedAuthorName = "Sebastian Galkin"
  , feedAuthorEmail = "paraseba@gmail.com"
  , feedRoot = baseUrl
  }

-- based on code from http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
-- replace a 2015-12-25-foo/index.markdown by foo/index.html
postRoute :: Routes
postRoute = customRoute createIndexRoute
  where
    createIndexRoute = (-<.> "html") . mapPath cleanup . toFilePath
    cleanup = dropWhile unwanted
    unwanted c = any ($c) [(== '-'), (== '_'), isDigit]

mapPath :: (String -> String) -> FilePath -> FilePath
mapPath f = joinPath . map f . splitPath

-- taken from http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url =
  case splitFileName url of
    (dir, "index.html")
      | isLocal dir -> dir
    _ -> url
  where
    isLocal uri = not (isInfixOf "://" uri)

myHakyllWriterOptions :: WriterOptions
myHakyllWriterOptions = defaultHakyllWriterOptions
    {writerExtensions = extensions}
  where
    extensions =
      disableExtension Ext_smart $ writerExtensions defaultHakyllWriterOptions
