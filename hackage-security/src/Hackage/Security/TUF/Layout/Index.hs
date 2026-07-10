module Hackage.Security.TUF.Layout.Index (
    -- * Repository layout
    IndexLayout(..)
  , IndexFile(..)
  , hackageIndexLayout
    -- ** Utility
  , indexLayoutPkgMetadata
  , indexLayoutPkgCabal
  , indexLayoutPkgPrefs
  ) where

import Prelude
import Data.Char (ord)
import Data.Kind (Type)
import Distribution.Package
import Distribution.Text
import Distribution.Types.Version (mkVersion)

import Hackage.Security.TUF.Paths
import Hackage.Security.TUF.Signed
import Hackage.Security.TUF.Targets
import Hackage.Security.Util.Path
import Hackage.Security.Util.Pretty
import Hackage.Security.Util.Some

{-------------------------------------------------------------------------------
  Index layout
-------------------------------------------------------------------------------}

-- | Layout of the files within the index tarball
data IndexLayout = IndexLayout  {
      -- | Translate an 'IndexFile' to a path
      indexFileToPath :: forall dec. IndexFile dec -> IndexPath

      -- | Parse an 'FilePath'
    , indexFileFromPath :: IndexPath -> Maybe (Some IndexFile)
    }

-- | Files that we might request from the index
--
-- The type index tells us the type of the decoded file, if any. For files for
-- which the library does not support decoding this will be @()@.
-- NOTE: Clients should NOT rely on this type index being @()@, or they might
-- break if we add support for parsing additional file formats in the future.
--
-- TODO: If we wanted to support legacy Hackage, we should also have a case for
-- the global preferred-versions file. But supporting legacy Hackage will
-- probably require more work anyway..
data IndexFile :: Type -> Type where
    -- Package-specific metadata (@targets.json@)
    IndexPkgMetadata :: PackageIdentifier -> IndexFile (Signed Targets)

    -- Cabal file for a package
    IndexPkgCabal :: PackageIdentifier -> IndexFile ()

    -- Preferred versions a package
    IndexPkgPrefs :: PackageName -> IndexFile ()
--TODO: ^^ older haddock doesn't support GADT doc comments :-(

deriving instance Show (IndexFile dec)

instance Pretty (IndexFile dec) where
  pretty (IndexPkgMetadata pkgId) = "metadata for " ++ display pkgId
  pretty (IndexPkgCabal    pkgId) = ".cabal for " ++ display pkgId
  pretty (IndexPkgPrefs    pkgNm) = "preferred-versions for " ++ display pkgNm

instance SomeShow   IndexFile where someShow   = DictShow
instance SomePretty IndexFile where somePretty = DictPretty

-- | The layout of the index as maintained on Hackage
hackageIndexLayout :: IndexLayout
hackageIndexLayout = IndexLayout {
      indexFileToPath   = toPath
    , indexFileFromPath = fromPath
    }
  where
    toPath :: IndexFile dec -> IndexPath
    toPath (IndexPkgCabal    pkgId) = fromFragments [
                                          display (packageName    pkgId)
                                        , display (packageVersion pkgId)
                                        , display (packageName pkgId) ++ ".cabal"
                                        ]
    toPath (IndexPkgMetadata pkgId) = fromFragments [
                                          display (packageName    pkgId)
                                        , display (packageVersion pkgId)
                                        , "package.json"
                                        ]
    toPath (IndexPkgPrefs    pkgNm) = fromFragments [
                                          display pkgNm
                                        , "preferred-versions"
                                        ]

    fromFragments :: [String] -> IndexPath
    fromFragments = rootPath . joinFragments

    fromPath :: IndexPath -> Maybe (Some IndexFile)
    fromPath fp = case splitFragments (unrootPath fp) of
      [pkg, version, _file] -> do
        let pkgName = mkPackageName pkg
            pkgVersion = mkVersion $ readVersion version
            pkgId = PackageIdentifier { pkgName, pkgVersion }
        case takeExtension fp of
          ".cabal"   -> return $ Some $ IndexPkgCabal    pkgId
          ".json"    -> return $ Some $ IndexPkgMetadata pkgId
          _otherwise -> Nothing
      [pkg, "preferred-versions"] ->
        Some . IndexPkgPrefs <$> simpleParse pkg
      _otherwise -> Nothing

-- Convert "3.12.1.0" to [3,12,1,0].
-- Copied from hackage-revdeps package.
readVersion :: String -> [Int]
readVersion = (\(acc, _mult, rest) -> acc : rest) . foldr go (0, 1, [])
  where
    go c (acc, mult, rest)
      | fromIntegral d < (10 :: Word) = (acc + d * mult, mult * 10, rest)
      | otherwise = (0, 1, acc : rest)
      where
        d = ord c - ord '0'

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

indexLayoutPkgMetadata :: IndexLayout -> PackageIdentifier -> IndexPath
indexLayoutPkgMetadata IndexLayout{..} = indexFileToPath . IndexPkgMetadata

indexLayoutPkgCabal :: IndexLayout -> PackageIdentifier -> IndexPath
indexLayoutPkgCabal IndexLayout{..} = indexFileToPath . IndexPkgCabal

indexLayoutPkgPrefs :: IndexLayout -> PackageName -> IndexPath
indexLayoutPkgPrefs IndexLayout{..} = indexFileToPath . IndexPkgPrefs
