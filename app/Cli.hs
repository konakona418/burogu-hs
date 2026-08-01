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
    | NewPost {nSlug :: Text, nDraft :: Bool}
    | Deploy
    | Sync {sAction :: Text, sRepo :: Maybe Text}

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
        <> command "new-post" (info (NewPost <$> slugParser <*> draftParser) (progDesc "Create a new post from a template"))
        <> command "deploy" (info (pure Deploy) (progDesc "Build and deploy the site (rsync or git, configured in config.yaml)"))
        <> command "sync" (info (Sync <$> actionParser <*> repoParser) (progDesc "Sync the site repository with a remote git repository"))

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

draftParser :: Parser Bool
draftParser = switch (long "draft" <> help "Create a draft (no date)")

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

versionOption :: Parser (a -> a)
versionOption =
    infoOption ("burogu " <> showVersion version) (long "version" <> help "Show version information")
