{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Chkfs where

--------------------------------------------------------------------------------

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           Data.Word
import           System.Exit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners

--------------------------------------------------------------------------------

_BSIZE :: Word32
_BSIZE = 1024


_FSMAGIC :: Word32
_FSMAGIC = 0x10203040


_IPB :: Word32
_IPB = 16


_NDIRECT :: Word32
_NDIRECT = 12

--------------------------------------------------------------------------------

parseImage :: String -> Word32 -> BL.ByteString -> Image
parseImage imgName imgSize = runGet (getImage imgName imgSize)

--------------------------------------------------------------------------------

data Image = Image
    { imgName    :: !String
    , imgSize    :: {-# UNPACK #-} !Word32
    , imgSb      :: !SuperBlock
    , imgDinodes :: !(V.Vector Dinode)
    }
    deriving (Show)


getImage :: String -> Word32 -> Get Image
getImage imgName imgSize = do
    -- boot block
    skip (fromIntegral _BSIZE)

    -- super block
    imgSb@SuperBlock{..} <- getSuperBlock

    -- log blocks
    skip (fromIntegral $ _BSIZE*sbNlog)

    -- inode blocks
    imgDinodes <-
        V.generateM (fromIntegral sbNinodes) (getDinode . fromIntegral)
    skip (fromIntegral $ _BSIZE*(sbBmapstart - sbInodestart) - 64*sbNinodes)

    pure Image{..}

--------------------------------------------------------------------------------

data SuperBlock = SuperBlock
    { sbMagic      :: {-# UNPACK #-} !Word32
    , sbSize       :: {-# UNPACK #-} !Word32
    , sbNblocks    :: {-# UNPACK #-} !Word32
    , sbNinodes    :: {-# UNPACK #-} !Word32
    , sbNlog       :: {-# UNPACK #-} !Word32
    , sbLogstart   :: {-# UNPACK #-} !Word32
    , sbInodestart :: {-# UNPACK #-} !Word32
    , sbBmapstart  :: {-# UNPACK #-} !Word32
    }
    deriving (Show)


getSuperBlock :: Get SuperBlock
getSuperBlock =
    SuperBlock
        <$> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*  skip (fromIntegral $ _BSIZE - 32*8)

--------------------------------------------------------------------------------

data FileType
    = Unknown
    | Dir
    | File
    | Dev
    deriving (Eq, Show)


data Dinode = Dinode
    { dinInum  :: {-# UNPACK #-} !Word32
    , dinType  :: !FileType
    , dinMajor :: {-# UNPACK #-} !Int16
    , dinMinor :: {-# UNPACK #-} !Int16
    , dinNlink :: {-# UNPACK #-} !Int16
    , dinSize  :: {-# UNPACK #-} !Word32
    , dinAddrs :: !(VU.Vector Word32)
    }
    deriving (Show)


getDinode :: Word32 -> Get Dinode
getDinode dinInum = do
    dinMajor <- getInt16host
    dinType <- toFileType <$> getInt16host
    dinNlink <- getInt16host
    dinMinor <- getInt16host
    dinSize <- getWord32host
    dinAddrs <- VU.replicateM (fromIntegral $ _NDIRECT + 1) getWord32host
    pure Dinode{..}

    where
        toFileType :: Int16 -> FileType
        toFileType = \case
            1 -> Dir
            2 -> File
            3 -> Dev
            _ -> Unknown

--------------------------------------------------------------------------------

runTest :: TestTree -> IO ()
runTest testTree = do
    installSignalHandlers

    sequence (tryIngredients defaultIngredients mempty testTree) >>= \case
        Just True -> do
            exitSuccess

        _ -> do
            exitFailure

--------------------------------------------------------------------------------

tests :: Image -> TestTree
tests img@Image{..} =
    testGroup imgName
        [ testsSb img
        ]


testsSb :: Image -> TestTree
testsSb Image{ imgSb = SuperBlock{..}, .. } =
    testGroup "super block"
        [ testCase "sbMagic should be _FSMAGIC" $
            sbMagic @?= _FSMAGIC

        , testCase "sbSize should be consistent" do
            sbSize @?= (imgSize `div` _BSIZE)
            sbSize @?= (nb + ns + nl + ni + nm + nd)

        , testCase "sbNblock should be consistent" $
            sbNblocks @?= nd

        , testCase "sbNlog should be consistent" $
            sbNlog @?= (sbInodestart - sbLogstart)

        , testCase "sbLogstart should be 2" $
            sbLogstart @?= 2

        , testCase "sbInodestart should be sbLogstart + sbNlog" $
            sbInodestart @?= (sbLogstart + sbNlog)

        , testCase "sbBmapstart should be sbInodestart + ni" $
            sbBmapstart @?= (sbInodestart + ni)
        ]
    where
        nb = 1
        ns = 1
        nl = sbNlog
        ni = sbNinodes `div` _IPB + 1
        nm = sbSize `div` (_BSIZE * 8) + 1
        nd = sbSize - (nb + ns + nl + ni + nm)
