--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char       (isDigit)
import           Data.List       (isInfixOf)
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import           Data.Time       (iso8601DateFormat)
import           Debug.Trace
import           Hakyll
import           System.FilePath (joinPath, splitFileName, splitPath, (-<.>))


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
                    constField "title" "Blog"                <>
                    baseContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
metaContextWithDefault :: String -> String -> Context String
metaContextWithDefault key defaultValue =
  field key $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe defaultValue $ M.lookup key metadata

baseContext :: Context String
baseContext =
  metaContextWithDefault "meta-description" "My personal blog. I write mostly about programming, particularly Haskell and other functional languages"
  <> metaContextWithDefault "meta-title" "Sebastian Galkin's Blog"
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
