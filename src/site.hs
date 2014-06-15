--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, mconcat)
import Hakyll
import GHC.IO.Encoding


--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyll $ do
    match ("*.png" .||. "passgen/*" .||. "files/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match ("news/*" .||. "doc/short/*" .||. "passgen/short/*") $ do
        route $ setExtension "html"
        compile pandocCompiler


    create ["index.html"] $ do
         route   idRoute
         compile $ do
           posts <- fmap (take 3) . recentFirst =<< loadAll "news/*"
           let indexCtx = mconcat
                          [ listField "posts" postCtx (return posts),
                            constField "title" "News",
                            constField "newsHighlight" "true",
                            defaultContext
                          ]
           makeItem ""
            >>= loadAndApplyTemplate "templates/newsList.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
         route   idRoute
         compile $ do
           posts <- fmap (drop 3) . recentFirst =<< loadAll "news/*"
           let indexCtx = mconcat
                          [ listField "posts" postCtx (return posts),
                            constField "title" "Archive",
                            constField "archiveHighlight" "true",
                            defaultContext
                          ]
           makeItem ""
            >>= loadAndApplyTemplate "templates/newsList.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    create ["doc/index.html"] $ do
         route   idRoute
         compile $ do
           posts <- recentFirst =<< loadAll "doc/short/*"
           let indexCtx = mconcat
                          [ listField "docs" postCtx (return posts),
                            constField "title" "Articles",
                            constField "articlesHighlight" "true",
                            defaultContext
                          ]
           makeItem ""
            >>= loadAndApplyTemplate "templates/docsList.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
    
    
    -- build up tags
    tags <- buildTags "doc/short/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = tag ++ " articles"
        route idRoute
        compile $ do
            docs <- recentFirst =<< loadAll pattern

            let ctx = mconcat
                          [ listField "docs" postCtx (return docs),
                            constField "title" title,
                            defaultContext
                          ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/docsListTag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
    
    match "about.markdown" $ do
        route $ setExtension "html"
        compile $ do
          let aboutCtx = mconcat [constField "aboutHighlight" "true", defaultContext]          
          pandocCompiler
            >>= loadAndApplyTemplate "templates/doc.html" aboutCtx
            >>= loadAndApplyTemplate "templates/default.html" aboutCtx
            >>= relativizeUrls
            
    match ("doc/*/*.markdown" .&&. complement "doc/short/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/doc.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            
    match ("doc/**.png" .||. "doc/**.jpg") $ do
    route   idRoute
    compile copyFileCompiler 

    create ["passgen/index.html"] $ do
      route   idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "passgen/short/*"
        let indexCtx = mconcat
                      [ listField "docs" postCtx (return posts),
                        constField "title" "Password generators",
                        constField "passgenHighlight" "true",
                        defaultContext
                      ]
        makeItem "" 
          >>= loadAndApplyTemplate "templates/passgenList.html" indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  let dateFieldCtx = dateField "date" "%B %e, %Y" 
  in mapContext (map repl) dateFieldCtx `mappend` defaultContext
    where repl '0' = 'o'
          repl n = n


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
