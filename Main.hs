{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Hakyll
import Text.Pandoc.SideNote

main :: IO ()
main = hakyll do
  match "vendor/tufte-css/tufte.css" do
    route idRoute
    compile compressCssCompiler
  match "et-book/*" do
    route idRoute
  match "css/*" $ compile compressCssCompiler
  create ["stylesheet.css"] do
    route idRoute
    compile do
      tufte <- load "vendor/tufte-css/tufte.css"
      csses <- loadAll "css/*.css"
      makeItem $ unlines $ map itemBody $ tufte : csses
  match "posts/*.org" do
    route $ setExtension "html"
    compile $
      customPandoc
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
  create ["archive"] $ do
    route $ setExtension "html"
    compile do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" defaultContext (pure posts)
              <> constField "title" "Archives"
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem @String "blog.sumtypeofway.com"
  match "about.org" do
    route $ setExtension "html"
    compile $
      customPandoc
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (constField "title" "About Me" <> defaultContext)
        >>= relativizeUrls
  match "contact.org" do
    route $ setExtension "html"
    compile $
      customPandoc
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (constField "title" "Get in Touch" <> defaultContext)
        >>= relativizeUrls
  match "index.html" do
    route idRoute
    compile do
      posts <- take 5 <$> (recentFirst =<< loadAll "posts/*")
      let ctx = listField "posts" defaultContext (pure posts) <> constField "title" "Home" <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
  match "templates/*" $ compile templateCompiler
  version "redirects" $
    createRedirects
      [ ("an-introduction-to-recursion-schemes/index.html", "/posts/introduction-to-recursion-schemes.html"),
        ("recursion-schemes-part-2/index.html", "/posts/recursion-schemes-part-2.html"),
        ("recursion-schemes-part-iii-folds-in-context/index.html", "posts/recursion-schemes-part-3.html"),
        ("recursion-schemes-part-iv-time-is-of-the-essence/index.html", "posts/recursion-schemes-part-4.html"),
        ("recursion-schemes-part-41-2-better-living-through-base-functors/index.html", "posts/recursion-schemes-part-4-point-5.html"),
        ("recursion-schemes-part-v/index.html", "posts/recursion-schemes-part-5.html"),
        ("recursion-schemes-part-6-comonads-composition-and-generality/index.html", "posts/recursion-schemes-part-6.html"),
        ("fluent-polymorphism-with-visible-type-applications/index.html", "/posts/fluent-polymorphism-type-applications.html")
      ]

customPandoc :: Compiler (Item String)
customPandoc = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions usingSideNotes
