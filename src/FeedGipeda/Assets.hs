{-# LANGUAGE TemplateHaskell #-}

module FeedGipeda.Assets
  ( defaultIndexHtml
  ) where


import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)


defaultIndexHtml :: ByteString
defaultIndexHtml =
  $(embedFile "assets/default_index.html")
