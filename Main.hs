{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Main where

import Hakyll

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
      makeItem $ unlines $ map itemBody $ tufte : csses


  match "posts/*.org" do
    route idRoute
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls


  match "index.html" do
    route idRoute
    compile do
      let ctx = constField "title" "Home" <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

