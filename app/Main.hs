module Main where

import Build (runBuild)
import Cli (Command (..), Paths (..), parseCommand)
import Deploy qualified
import Doc qualified
import Format qualified
import Image qualified
import Init qualified
import Posts qualified
import Sync qualified
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import Watch (runPreview, runWatch)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    command <- parseCommand
    case command of
        Build paths -> runBuild paths
        Clean outDir -> removePathForcibly outDir
        Preview port -> runPreview port
        Watch mServe -> runWatch mServe
        Init dir -> Init.run dir
        New slug -> Posts.orDie (Posts.runNew "src/_post" slug)
        Draft slug -> Posts.orDie (Posts.runDraft "src/_post" slug)
        Publish slug -> Posts.orDie (Posts.runPublish "src/_post" slug)
        Rename old new -> Posts.orDie (Posts.runRename "src/_post" old new)
        Image digest mFile mQ mD clipboard paths -> Posts.orDie (Image.runImage (pSrc paths </> "_post") digest mFile mQ mD clipboard)
        Deploy clear log_ -> Deploy.run clear log_
        Sync action mRepo log_ -> Sync.run action mRepo log_
        Format dryRun paths -> Format.run paths dryRun
        Doc section lang color -> Doc.run section lang color
