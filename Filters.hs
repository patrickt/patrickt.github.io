{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Filters (usingOldstyleSyntax) where

import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Walk (walk)

usingOldstyleSyntax :: Pandoc -> Pandoc
usingOldstyleSyntax = walk go
  where
    go :: Inline -> Inline
    go (Strong [Str (T.uncons -> Just (':', num))]) =
      Span ("", ["numeral"], []) [Str num]
    go x =
      x
