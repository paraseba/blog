--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
import           Data.Monoid ((<>))
import           Data.List (isInfixOf)
import           Data.Char (isDigit)
import           Hakyll
import           Debug.Trace
import           Data.Time (iso8601DateFormat)
import           System.FilePath (joinPath, splitPath, splitFileName, (-<.>))


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
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    dateField "isoDate" (iso8601DateFormat Nothing) <>
    defaultContext

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
