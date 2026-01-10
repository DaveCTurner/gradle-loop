{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Help (showHelp) where

import HelpTemplate
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

$(loadRawHelpTextBytes)

showHelp :: IO ()
showHelp = do
    let !(RawBytes addr len) = rawHelpTextBytes
    helpText <- B.unsafePackLenAddress len addr
    B.putStr helpText
