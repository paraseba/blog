--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Control.Applicative as A
import           Data.Char           (isDigit)
import           Data.List           (isInfixOf)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>), First(..))
import           Data.Time           (iso8601DateFormat)
import           Debug.Trace
import           Hakyll
import           System.FilePath     (joinPath, splitFileName, splitPath,
                                      (-<.>))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown"]) $ do
        route   $ constRoute "about/index.html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" baseContext
            >>= relativizeUrls
            >>= removeIndexHtml

    match "posts/*/index.markdown" $ do
        route $ postRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) <>
    --                 constField "title" "Archives"            <>
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*/index.markdown"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    baseContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- |Create a Context with key and value given by the functino f applied to
-- the item metadata. If f returns Nothing an empty field will be created
withMetaContext :: String -> (Metadata -> Maybe String) -> Context String
withMetaContext key f = Context $ \k args i ->
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
metaDefaultContext key defaultKeys defaultValue =
  withMetaContext key getKey
  where
    getKey :: Metadata -> Maybe String
    getKey meta = foldr1 (A.<|>) options
      where options = map (`M.lookup` meta) (key:defaultKeys) ++ [defaultValue]

defaultDescription :: String
defaultDescription = "My personal blog. I write mostly about programming, particularly Haskell and other functional languages"

defaultTitle :: String
defaultTitle = "Sebastian Galkin's Blog"

baseContext :: Context String
baseContext =
  metaDefaultContext "meta-description" ["description"] (Just defaultDescription)
  <> metaDefaultContext "meta-title" ["title"] (Just defaultTitle)
  <> defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    dateField "isoDate" (iso8601DateFormat Nothing) <>
    baseContext

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
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") | isLocal dir -> init dir
        _                                 -> url
        where isLocal uri = not (isInfixOf "://" uri)
