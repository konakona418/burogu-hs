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
    | Deploy {dClearCache :: Bool}
    | Sync {sAction :: Text, sRepo :: Maybe Text}
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
        <> command "preview" (info (Preview <$> portParser) (progDesc "Build once, then serve the site locally"))
        <> command "watch" (info (Watch <$> serveParser) (progDesc "Rebuild when sources change, optionally serve"))
        <> command "init" (info (Init <$> dirParser) (progDesc "Initialize an src/ tree"))
        <> command "new" (info (New <$> slugParser) (progDesc "Create a new post (dated with today's date)"))
        <> command "draft" (info (Draft <$> slugParser) (progDesc "Create a draft (draft: true, not published)"))
        <> command "publish" (info (Publish <$> slugParser) (progDesc "Publish a draft (adds the date, removes the draft flag)"))
        <> command "deploy" (info (Deploy <$> clearCacheParser) (progDesc "Build and deploy the site (rsync or git, configured in config.yaml)"))
        <> command "sync" (info (Sync <$> actionParser <*> repoParser) (progDesc "Sync the site repository with a remote git repository"))
        <> command "format" (info (Format <$> dryRunParser <*> pathsParser) (progDesc "Normalize config.yaml, posts and pages (frontmatter defaults, canonical order)"))
        <> command "doc" (info (Doc <$> sectionParser <*> langParser <*> colorParser) (progDesc "Print the man-style manual (en/zh, sections, ANSI styling)"))

pathsParser :: Parser Paths
pathsParser =
    Paths
        <$> strOption
            ( long "config"
                <> metavar "PATH"
                <> value "config.yaml"
                <> showDefault
                <> help "Path to the site configuration file"
            )
        <*> strOption
            ( long "src"
                <> metavar "DIR"
                <> value "src"
                <> showDefault
                <> help "Source directory (posts live in DIR/_post)"
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
                <> help "Also serve the site on this port"
            )
        )

dirParser :: Parser FilePath
dirParser =
    strArgument
        ( metavar "DIR"
            <> value "src"
            <> help "Target directory (default: src)"
        )

slugParser :: Parser Text
slugParser = strArgument (metavar "SLUG" <> help "Post slug (used in the filename and URL)")

clearCacheParser :: Parser Bool
clearCacheParser = switch (long "clear-cache" <> help "Remove the persistent git cache (next deploy re-fetches from scratch)")

dryRunParser :: Parser Bool
dryRunParser = switch (long "dry-run" <> help "Show what would change without writing anything")

actionParser :: Parser Text
actionParser = strArgument (metavar "ACTION" <> help "push or pull")

repoParser :: Parser (Maybe Text)
repoParser =
    optional
        ( strArgument
            ( metavar "REPO"
                <> help "git repo URL (defaults to srcRepo in config.yaml)"
            )
        )

sectionParser :: Parser (Maybe Text)
sectionParser =
    optional
        ( strArgument
            ( metavar "SECTION"
                <> help "Manual section to print (default: the whole manual)"
            )
        )

langParser :: Parser (Maybe Text)
langParser =
    optional
        ( strOption
            ( long "lang"
                <> metavar "LANG"
                <> help "Manual language: en or zh (default: follow the locale)"
            )
        )

colorParser :: Parser (Maybe Text)
colorParser =
    optional
        ( strOption
            ( long "color"
                <> metavar "MODE"
                <> help "ANSI styling: auto, always or never (default: auto)"
            )
        )

versionOption :: Parser (a -> a)
versionOption =
    infoOption ("burogu " <> showVersion version) (long "version" <> help "Show version information")
