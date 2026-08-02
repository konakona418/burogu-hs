module Main where

import Build (runBuild)
import Cli (Command (..), parseCommand)
import Deploy qualified
import Doc qualified
import Format qualified
import Init qualified
import Posts qualified
import Sync qualified
import System.Directory (removePathForcibly)
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
        Deploy clear -> Deploy.run clear
        Sync action mRepo -> Sync.run action mRepo
        Format dryRun paths -> Format.run paths dryRun
        Doc section lang color -> Doc.run section lang color
