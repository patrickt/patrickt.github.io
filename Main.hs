{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Main (main) where

import Hakyll
import Debug.Trace

main :: IO ()
main = hakyll do
  match "vendor/tufte-css/tufte.css" do
    route   idRoute
    compile compressCssCompiler

  create ["stylesheet.css"] do
    route idRoute
    compile do
      tufte <- load "vendor/tufte-css/tufte.css"
      csses <- loadAll "css/*.css"
      traceShowM csses
      makeItem $ unlines $ map itemBody $ tufte : csses


  match "posts/*.org" do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls


  match "index.html" do
    route idRoute
    compile do
      posts <- recentFirst =<< loadAll "posts/*"
      let ctx = listField "posts" defaultContext (pure posts) <> constField "title" "Home" <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
