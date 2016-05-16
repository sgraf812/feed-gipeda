{-# LANGUAGE TemplateHaskell #-}

{-| Contains @ByteString@ assets embedded via
    @<https://hackage.haskell.org/package/file-embed file-embed>@.
-}

module FeedGipeda.Assets
  ( defaultIndexHtml
  ) where


import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile, makeRelativeToProject)


{-| A default @index.html@ to use as the top-level index site, as long as
    the user doesn't paste its own to the appropriate location
    (e.g. @`pwd`/default_index.html@).
-}
defaultIndexHtml :: ByteString
defaultIndexHtml =
  $(makeRelativeToProject "assets/default_index.html" >>= embedFile)
