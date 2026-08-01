module Main where

import Build (runBuild)
import Cli (Command (..), parseCommand)
import Deploy qualified
import Doc qualified
import Format qualified
import Init qualified
import NewPost qualified
import Sync qualified
import System.Directory (removePathForcibly)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Watch (runPreview, runWatch)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    command <- parseCommand
    case command of
        Build paths -> runBuild paths
        Clean outDir -> removePathForcibly outDir
        Preview port -> runPreview port
        Watch mServe -> runWatch mServe
        Init dir -> Init.run dir
        NewPost slug draft -> NewPost.run slug draft
        Deploy clear -> Deploy.run clear
        Sync action mRepo -> Sync.run action mRepo
        Format dryRun paths -> Format.run paths dryRun
        Doc section lang color -> Doc.run section lang color
