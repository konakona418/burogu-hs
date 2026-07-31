module Cli (Paths (..), parsePaths, pathsInfo) where

import Data.Version (showVersion)
import Options.Applicative
import Paths_burogu (version)

data Paths = Paths
    { pConfig :: FilePath
    , pSrc :: FilePath
    , pOut :: FilePath
    }

parsePaths :: IO Paths
parsePaths = execParser pathsInfo

pathsInfo :: ParserInfo Paths
pathsInfo =
    info
        (helper <*> versionOption <*> pathsParser)
        ( fullDesc
            <> progDesc "Generate a static blog site from src/ into site/"
            <> header "burogu - a static blog generator"
        )

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

versionOption :: Parser (a -> a)
versionOption =
    infoOption ("burogu " <> showVersion version) (long "version" <> help "Show version information")
