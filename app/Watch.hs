module Watch (contentType, parsePath, resolveFile, runPreview, runWatch) where

import Build (runBuild)
import Cli (defaultPaths)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word8)
import Network.Socket (
    AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    AddrInfoFlag (AI_PASSIVE),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    addrFlags,
    bind,
    close,
    defaultHints,
    getAddrInfo,
    listen,
    setSocketOption,
    socket,
 )
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath (splitDirectories, takeExtension, (</>))

pollInterval :: Int
pollInterval = 2000000

rebuild :: IO ()
rebuild = do
    TIO.putStrLn "--- rebuilding ---"
    result <- try (runBuild defaultPaths) :: IO (Either ExitCode ())
    case result of
        Left _ -> TIO.putStrLn "build failed; keeping the previous output"
        Right () -> pure ()

serveSite :: Int -> IO ()
serveSite port = do
    TIO.putStrLn ("Preview: http://127.0.0.1:" <> T.pack (show port) <> "/  (Ctrl-C to stop)")
    addrs <-
        getAddrInfo
            (Just defaultHints{addrFlags = [AI_PASSIVE], addrSocketType = Stream})
            (Just "127.0.0.1")
            (Just (show port))
    let addr = head addrs
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 16
    forever $ do
        (conn, _) <- accept sock
        handleRequest conn `finally` close conn
  where
    handleRequest :: Socket -> IO ()
    handleRequest conn = do
        req <- recv conn 4096
        case parsePath req >>= resolveFile "site" of
            Just file -> do
                exists <- doesFileExist file
                if exists
                    then BS.readFile file >>= \body -> sendAll conn (okResponse file body)
                    else serveNotFound conn
            Nothing -> serveNotFound conn
    serveNotFound :: Socket -> IO ()
    serveNotFound conn = do
        let missing = "site" </> "404.html"
        exists <- doesFileExist missing
        if exists
            then BS.readFile missing >>= \body -> sendAll conn (response "404 Not Found" missing body)
            else sendAll conn notFound

{- | Parse the request target from a request line: percent-decoded path
with the query string stripped. Returns Nothing for non-GET requests.
-}
parsePath :: BS.ByteString -> Maybe BS.ByteString
parsePath req =
    case BS.split 32 (BS.takeWhile (/= 10) req) of
        [method, target, _]
            | method == "GET" -> Just (percentDecode (BS.takeWhile (/= 63) target))
        _ -> Nothing

{- | Resolve a request path against the output directory, guarding
against path traversal. Directory URLs get index.html appended.
-}
resolveFile :: FilePath -> BS.ByteString -> Maybe FilePath
resolveFile outDir path
    | BS.null path || path == "/" = Just (outDir </> "index.html")
    | otherwise = do
        let raw = T.unpack (decodeUtf8Lenient path)
            rel = dropWhile (== '/') raw
            relBase = reverse (dropWhile (== '/') (reverse rel))
            rel' = if rel /= relBase then relBase <> "/index.html" else rel
        if any (== "..") (splitDirectories rel')
            then Nothing
            else Just (outDir </> rel')

percentDecode :: BS.ByteString -> BS.ByteString
percentDecode = BS.pack . go . BS.unpack
  where
    go (37 : a : b : rest) -- '%'
        | isHex a && isHex b = fromIntegral (hexVal a * 16 + hexVal b) : go rest
    go (c : rest) = c : go rest
    go [] = []
    isHex :: Word8 -> Bool
    isHex w = (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
    hexVal :: Word8 -> Word8
    hexVal w
        | w >= 48 && w <= 57 = w - 48
        | w >= 97 && w <= 102 = w - 97 + 10
        | otherwise = w - 65 + 10

okResponse :: FilePath -> BS.ByteString -> BS.ByteString
okResponse = response "200 OK"

response :: BS.ByteString -> FilePath -> BS.ByteString -> BS.ByteString
response status file body =
    "HTTP/1.1 "
        <> status
        <> "\r\nContent-Type: "
        <> encodeUtf8 (contentType file)
        <> "\r\nContent-Length: "
        <> encodeUtf8 (T.pack (show (BS.length body)))
        <> "\r\nConnection: close\r\n\r\n"
        <> body

notFound :: BS.ByteString
notFound = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain; charset=utf-8\r\nConnection: close\r\n\r\nnot found\n"

contentType :: FilePath -> T.Text
contentType file = case takeExtension file of
    ".html" -> "text/html; charset=utf-8"
    ".css" -> "text/css; charset=utf-8"
    ".js" -> "text/javascript; charset=utf-8"
    ".png" -> "image/png"
    ".jpg" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".gif" -> "image/gif"
    ".svg" -> "image/svg+xml"
    ".webp" -> "image/webp"
    ".ico" -> "image/x-icon"
    ".xml" -> "application/xml"
    ".txt" -> "text/plain; charset=utf-8"
    ".json" -> "application/json"
    ".woff2" -> "font/woff2"
    _ -> "application/octet-stream"

runPreview :: Int -> IO ()
runPreview port = do
    rebuild
    serveSite port

runWatch :: Maybe Int -> IO ()
runWatch mPort = do
    case mPort of
        Just port -> serveSite port
        Nothing -> pure ()
    lastMtime <- latestMtime
    rebuild
    TIO.putStrLn "Watching src/ and config.yaml... (Ctrl-C to stop)"
    loop lastMtime
  where
    loop :: UTCTime -> IO ()
    loop lastMtime = do
        threadDelay pollInterval
        current <- latestMtime
        if current > lastMtime
            then do
                rebuild
                loop current
            else loop lastMtime

{- | The newest modification time among src/ (recursively) and config.yaml,
falling back to the current time when nothing is readable.
-}
latestMtime :: IO UTCTime
latestMtime = do
    now <- getCurrentTime
    files <- collectFiles "src" <&> (++ ["config.yaml"])
    times <- mapM (\f -> try (getModificationTime f) :: IO (Either SomeException UTCTime)) files
    let readable = [t | Right t <- times]
    pure (if null readable then now else foldl1 max readable)

collectFiles :: FilePath -> IO [FilePath]
collectFiles dir = do
    isDir <- doesDirectoryExist dir
    if not isDir
        then pure [dir]
        else do
            entries <- listDirectory dir
            fmap concat . mapM (\e -> collectFiles (dir </> e)) $ entries
