module Cli (Command (..), Paths (..), defaultPaths, parseCommand, cliInfo) where

import Data.Text (Text)
import Data.Version (showVersion)
import Options.Applicative
import Paths_burogu (version)

data Paths = Paths
    { pConfig :: FilePath
    , pSrc :: FilePath
    , pOut :: FilePath
    }

defaultPaths :: Paths
defaultPaths = Paths{pConfig = "config.yaml", pSrc = "src", pOut = "site"}

data Command
    = Build {cPaths :: Paths}
    | Clean {cOut :: FilePath}
    | Preview {pPort :: Int}
    | Watch {wServe :: Maybe Int}
    | Init {iDir :: FilePath}
    | New {nSlug :: Text}
    | Draft {dSlug :: Text}
    | Publish {pSlug :: Text}
    | Rename {rOldSlug :: Text, rNewSlug :: Text}
    | Image {iDigest :: Text, iFile :: FilePath, iQuality :: Maybe Int, iMaxDim :: Maybe Int, iPaths :: Paths}
    | Deploy {dClearCache :: Bool, dLog :: Bool}
    | Sync {sAction :: Text, sRepo :: Maybe Text, sLog :: Bool}
    | Format {fDryRun :: Bool, fPaths :: Paths}
    | Doc {dSection :: Maybe Text, dLang :: Maybe Text, dColor :: Maybe Text}

parseCommand :: IO Command
parseCommand = execParser cliInfo

cliInfo :: ParserInfo Command
cliInfo =
    info
        (helper <*> versionOption <*> subparser commands)
        ( fullDesc
            <> progDesc "A static blog generator"
            <> header "burogu"
        )

commands :: Mod CommandFields Command
commands =
    command "build" (info (Build <$> pathsParser) (progDesc "Build the site"))
        <> command "clean" (info (Clean <$> outParser) (progDesc "Remove the output directory"))
        <> command "preview" (info (Preview <$> portParser) (progDesc "Build once, then serve the site"))
        <> command "watch" (info (Watch <$> serveParser) (progDesc "Rebuild on change, optionally serve"))
        <> command "init" (info (Init <$> dirParser) (progDesc "Initialize an src/ tree"))
        <> command "new" (info (New <$> slugParser) (progDesc "Create a post"))
        <> command "draft" (info (Draft <$> slugParser) (progDesc "Create a draft"))
        <> command "publish" (info (Publish <$> slugParser) (progDesc "Publish a draft"))
        <> command "rename" (info (Rename <$> slugParser <*> slugParser) (progDesc "Rename a post (file name only)"))
        <> command "image" (info (Image <$> digestArg <*> fileArg <*> qualityOption <*> maxDimOption <*> pathsParser) (progDesc "Compress an image into the site (src/img/<digest>/)"))
        <> command "deploy" (info (Deploy <$> clearCacheParser <*> logParser) (progDesc "Build and deploy the site"))
        <> command "sync" (info (Sync <$> actionParser <*> repoParser <*> logParser) (progDesc "Sync the site repository with a git remote"))
        <> command "sync" (info (Sync <$> actionParser <*> repoParser <*> logParser) (progDesc "Sync the site repository with a git remote"))
        <> command "format" (info (Format <$> dryRunParser <*> pathsParser) (progDesc "Normalize config, posts and pages"))
        <> command "doc" (info (Doc <$> sectionParser <*> langParser <*> colorParser) (progDesc "Print the manual"))

pathsParser :: Parser Paths
pathsParser =
    Paths
        <$> strOption
            ( long "config"
                <> metavar "PATH"
                <> value "config.yaml"
                <> showDefault
                <> help "Site configuration file"
            )
        <*> strOption
            ( long "src"
                <> metavar "DIR"
                <> value "src"
                <> showDefault
                <> help "Source directory"
            )
        <*> strOption
            ( long "out"
                <> metavar "DIR"
                <> value "site"
                <> showDefault
                <> help "Output directory"
            )

outParser :: Parser FilePath
outParser =
    strOption
        ( long "out"
            <> metavar "DIR"
            <> value "site"
            <> showDefault
            <> help "Output directory"
        )

portParser :: Parser Int
portParser =
    option
        auto
        ( long "port"
            <> metavar "PORT"
            <> value 8000
            <> showDefault
            <> help "Port to serve on"
        )

serveParser :: Parser (Maybe Int)
serveParser =
    optional
        ( option
            auto
            ( long "serve"
                <> metavar "PORT"
                <> help "Also serve on this port"
            )
        )

dirParser :: Parser FilePath
dirParser =
    strArgument
        ( metavar "DIR"
            <> value "src"
            <> help "Target directory"
        )

slugParser :: Parser Text
slugParser = strArgument (metavar "SLUG" <> help "Post slug")

logParser :: Parser Bool
logParser = switch (long "log" <> help "Show git's own verbose output (pushes, fetches)")

clearCacheParser :: Parser Bool
clearCacheParser = switch (long "clear-cache" <> help "Clear the git deploy cache")

digestArg :: Parser Text
digestArg = strArgument (metavar "DIGEST" <> help "The post digest (8 hex chars)")

fileArg :: Parser FilePath
fileArg = strArgument (metavar "FILE" <> help "Image file to compress")

qualityOption :: Parser (Maybe Int)
qualityOption = optional (option auto (long "quality" <> metavar "N" <> help "JPEG quality (default 85)"))

maxDimOption :: Parser (Maybe Int)
maxDimOption = optional (option auto (long "max-dim" <> metavar "N" <> help "Longest edge in pixels (default 1600)"))

dryRunParser :: Parser Bool
dryRunParser = switch (long "dry-run" <> help "Show changes without writing")

actionParser :: Parser Text
actionParser = strArgument (metavar "ACTION" <> help "push or pull")

repoParser :: Parser (Maybe Text)
repoParser =
    optional
        ( strArgument
            ( metavar "REPO"
                <> help "Git repository URL"
            )
        )

sectionParser :: Parser (Maybe Text)
sectionParser =
    optional
        ( strArgument
            ( metavar "SECTION"
                <> help "Manual section"
            )
        )

langParser :: Parser (Maybe Text)
langParser =
    optional
        ( strOption
            ( long "lang"
                <> metavar "LANG"
                <> help "Manual language"
            )
        )

colorParser :: Parser (Maybe Text)
colorParser =
    optional
        ( strOption
            ( long "color"
                <> metavar "MODE"
                <> help "ANSI styling"
            )
        )

versionOption :: Parser (a -> a)
versionOption =
    infoOption ("burogu " <> showVersion version) (long "version" <> help "Show version information")
