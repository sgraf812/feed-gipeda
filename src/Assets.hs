{-# LANGUAGE TemplateHaskell #-}

module Assets
  ( defaultIndexHtml
  ) where


import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)


defaultIndexHtml :: ByteString
defaultIndexHtml =
  $(embedFile "assets/default_index.html")
