module Image (runImage) where

import Codec.Picture (
    DynamicImage,
    Image,
    PixelRGBA8,
    convertRGBA8,
    decodeImage,
    encodeJpegAtQuality,
    generateImage,
    imageHeight,
    imageWidth,
    pixelAt,
    readImage,
 )
import Codec.Picture.Types (PixelRGBA8 (..), PixelRGB8 (..), convertImage)
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word8)
import Digest (digestOf, fnv1a)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, findExecutable, getFileSize, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, std_err, std_out, waitForProcess)

{- | Add an image to the site: compress it (JPEG), store it under
src/img/<digest>/ named by its content hash, print the compression
ratio and a copy-pasteable markdown embed. With --clipboard the image
is read from the wayland clipboard (wl-paste) instead of a file.
-}
runImage :: FilePath -> Text -> Maybe FilePath -> Maybe Int -> Maybe Int -> Bool -> IO (Either Text ())
runImage postDir digest mImageFile mQuality mMaxDim clipboard = do
    eName <- findPost postDir digest
    case eName of
        Left err -> pure (Left err)
        Right _ -> case (mImageFile, clipboard) of
            (Just _, True) -> pure (Left "FILE and --clipboard are mutually exclusive")
            (Nothing, False) -> pure (Left "missing image FILE (or pass --clipboard)")
            (Just f, False) -> do
                edecoded <- readImage f
                case edecoded of
                    Left err -> pure (Left (T.pack err))
                    Right dyn -> do
                        oldSize <- getFileSize f
                        processImage postDir digest (T.pack f) (T.pack (takeBaseName f)) dyn oldSize mQuality mMaxDim
            (Nothing, True) -> do
                eimg <- getClipboardImage
                case eimg of
                    Left err -> pure (Left err)
                    Right (dyn, n) -> processImage postDir digest "<clipboard>" "screenshot" dyn (fromIntegral n) mQuality mMaxDim

{- | Compress, store and report one image. Shared by the file and the
clipboard input paths.
-}
processImage :: FilePath -> Text -> Text -> Text -> DynamicImage -> Integer -> Maybe Int -> Maybe Int -> IO (Either Text ())
processImage postDir digest displayName alt dyn oldSize mQuality mMaxDim = do
    let img = convertRGBA8 dyn
        w = imageWidth img
        h = imageHeight img
        maxDim = fromMaybe 1600 mMaxDim
        quality = fromIntegral (fromMaybe 85 mQuality) :: Word8
        (nw, nh) = fit w h maxDim
        scaled = if nw == w && nh == h then img else resizeBilinear nw nh img
        jpeg = LBS.toStrict (encodeJpegAtQuality quality (convertImage (toRGB8 scaled)))
        hashName = fnv1aBytes jpeg <> ".jpg"
        dir = takeDirectory postDir </> "img" </> T.unpack digest
        outPath = dir </> T.unpack hashName
    createDirectoryIfMissing True dir
    BS.writeFile outPath jpeg
    newSize <- getFileSize outPath
    let ratio = if oldSize > 0 then (1 - fromIntegral newSize / fromIntegral oldSize) * 100 :: Double else 0
    TIO.putStrLn ("image: " <> displayName <> " (" <> T.pack (show w) <> "x" <> T.pack (show h) <> " -> " <> T.pack (show nw) <> "x" <> T.pack (show nh) <> ")")
    TIO.putStrLn ("  compressed " <> T.pack (show (round ratio :: Int)) <> "%  " <> T.pack (show oldSize) <> "B -> " <> T.pack (show newSize) <> "B")
    TIO.putStrLn ("  " <> markdownEmbed alt digest hashName)
    pure (Right ())

{- | Read the image from the wayland clipboard: try the common image
MIME types in order and decode the first one that yields an image.
-}
getClipboardImage :: IO (Either Text (DynamicImage, Int))
getClipboardImage = do
    exists <- doesExecutableExist "wl-paste"
    if not exists
        then pure (Left "wl-paste not found; install wl-clipboard")
        else do
            results <- forM clipboardMimes $ \mime -> do
                (code, bytes) <- runWlPaste mime
                if code == ExitSuccess && not (BS.null bytes)
                    then pure (Just (decodeImage bytes, BS.length bytes))
                    else pure Nothing
            case catMaybes results of
                ((Right dyn, n) : _) -> pure (Right (dyn, n))
                [] -> pure (Left "clipboard has no image")
                _ -> pure (Left "clipboard image could not be decoded")

clipboardMimes :: [String]
clipboardMimes = ["image/png", "image/jpeg", "image/bmp", "image/tiff", "image/webp"]

runWlPaste :: String -> IO (ExitCode, BS.ByteString)
runWlPaste mime = do
    (_, Just outH, _, ph) <- createProcess (proc "wl-paste" ["--type", mime]){std_out = CreatePipe, std_err = CreatePipe}
    bytes <- BS.hGetContents outH
    code <- waitForProcess ph
    pure (code, bytes)

doesExecutableExist :: String -> IO Bool
doesExecutableExist name = do
    found <- findExecutable name
    pure (maybe False (const True) found)

markdownEmbed :: Text -> Text -> Text -> Text
markdownEmbed alt digest hashName =
    "![" <> alt <> "](/img/" <> digest <> "/" <> hashName <> ")"

{- | Find the post whose digest matches, returning its file name.
-}
findPost :: FilePath -> Text -> IO (Either Text FilePath)
findPost dir digest = do
    isDir <- doesDirectoryExist dir
    if not isDir
        then pure (Left ("no post directory at " <> T.pack dir))
        else do
            names <- filter (T.isSuffixOf ".md" . T.pack) <$> listDirectory dir
            case [n | n <- names, digestOf (T.dropEnd 3 (T.pack n)) == digest] of
                [n] -> pure (Right n)
                [] -> pure (Left ("no post with digest '" <> digest <> "' in " <> T.pack dir))
                _ -> pure (Left ("multiple posts share digest '" <> digest <> "'"))

toRGB8 :: Image PixelRGBA8 -> Image PixelRGB8
toRGB8 img = generateImage (\x y -> let PixelRGBA8 r g b _ = pixelAt img x y in PixelRGB8 r g b) (imageWidth img) (imageHeight img)

{- | Bilinear scaling for RGBA images (JuicyPixels 3.3 has no built-in
resampler).
-}
resizeBilinear :: Int -> Int -> Image PixelRGBA8 -> Image PixelRGBA8
resizeBilinear nw nh src = generateImage sample nw nh
  where
    sw = imageWidth src
    sh = imageHeight src
    at x y = pixelAt src x y
    sample x y =
        let gx = fromIntegral x * fromIntegral (sw - 1) / fromIntegral (max 1 (nw - 1)) :: Double
            gy = fromIntegral y * fromIntegral (sh - 1) / fromIntegral (max 1 (nh - 1)) :: Double
            x0 = floor gx
            y0 = floor gy
            x1 = min (x0 + 1) (sw - 1)
            y1 = min (y0 + 1) (sh - 1)
            dx = gx - fromIntegral x0
            dy = gy - fromIntegral y0
            PixelRGBA8 r00 g00 b00 a00 = at x0 y0
            PixelRGBA8 r10 g10 b10 a10 = at x1 y0
            PixelRGBA8 r01 g01 b01 a01 = at x0 y1
            PixelRGBA8 r11 g11 b11 a11 = at x1 y1
            lerp v00 v10 v01 v11 =
                fromIntegral (round (fromIntegral v00 * (1 - dx) * (1 - dy) + fromIntegral v10 * dx * (1 - dy) + fromIntegral v01 * (1 - dx) * dy + fromIntegral v11 * dx * dy) :: Int) :: Word8
         in PixelRGBA8
                (lerp r00 r10 r01 r11)
                (lerp g00 g10 g01 g11)
                (lerp b00 b10 b01 b11)
                (lerp a00 a10 a01 a11)

fit :: Int -> Int -> Int -> (Int, Int)
fit w h maxDim
    | w <= maxDim && h <= maxDim = (w, h)
    | w >= h = (maxDim, max (round (fromIntegral h * fromIntegral maxDim / fromIntegral w :: Double)) 1)
    | otherwise = (max (round (fromIntegral w * fromIntegral maxDim / fromIntegral h :: Double)) 1, maxDim)

fnv1aBytes :: BS.ByteString -> Text
fnv1aBytes = fnv1a . T.pack . map word8ToChar . BS.unpack
  where
    word8ToChar :: Word8 -> Char
    word8ToChar = toEnum . fromIntegral