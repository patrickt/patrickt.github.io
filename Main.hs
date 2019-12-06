{-# LANGUAGE BlockArguments #-}

module Main where

import Hakyll

main :: IO ()
main = hakyll do
  match "index.html" do
    route idRoute
    compile do
      let ctx = constField "title" "Home" <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

