import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

compileErlang :: CmdResult r => FilePath -> FilePath -> FilePath -> FilePath -> Action r
compileErlang file include includeLib outputDir =
    cmd "erlc" "-o" outputDir "-I" include "-I" includeLib file

runShake :: IO ()
runShake = shakeArgs shakeOptions $ do

    phony "clean" $ do
        removeFilesAfter "ebin" ["//*"]

    "ebin/*.beam" *> \out -> do
        let src = "src" </> (dropDirectory1 $ out) -<.> "erl"
        need [src]
        compileErlang src "include" "deps" "ebin"

    "deps/*/ebin/*.beam" *> \out -> do
        let (dir, name) = splitFileName out
            src = (takeDirectory $ takeDirectory dir) </> "src" </> name -<.> "erl"
        need [src]
        let ebinDir = takeDirectory out
            includeDir = (takeDirectory $ takeDirectory out) </> "include"
        compileErlang src includeDir "deps" ebinDir

    "compile" ~> do
        cs <- getDirectoryFiles "src" ["*.erl"]
        depCs <- getDirectoryFiles "deps" ["*/src/*.erl"]
        let os = ["ebin" </> c -<.> "beam" | c <- cs]
            dos = ["deps" </> (takeDirectory1 d) </> "ebin" </> (dropDirectory1 $ dropDirectory1 d) -<.> "beam" | d <- depCs]
        need $ os ++ dos

main :: IO ()
main = runShake
