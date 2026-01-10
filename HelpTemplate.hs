{-# LANGUAGE MagicHash #-}

module HelpTemplate (loadRawHelpTextBytes, RawBytes(..)) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.Prim

data RawBytes = RawBytes Addr# Int

loadRawHelpTextBytes :: Q [Dec]
loadRawHelpTextBytes = do
  addDependentFile "help-source.txt"
  runIO $ do
    (foreignPtr, offset, len) <- B.toForeignPtr <$> B.readFile "help-source.txt"
    return
      [ SigD (Name (OccName "rawHelpTextBytes") NameS) (ConT (Name (OccName "RawBytes") NameS))
      , ValD
          (VarP (Name (OccName "rawHelpTextBytes") NameS))
          (NormalB (AppE (AppE (ConE (Name (OccName "RawBytes") NameS))
                               (LitE (bytesPrimL (mkBytes foreignPtr (fromIntegral offset) (fromIntegral len)))))
                         (LitE $ IntegerL $ fromIntegral len)))
          []
      ]
