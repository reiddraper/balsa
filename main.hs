module Main where

import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import System.IO
import System.Process
import Development.Shake
import Development.Shake.FilePath

type ErlangCompiler = FilePath -> FilePath -> FilePath -> FilePath -> Action ()

data CompileRequest = CompileRequest {
    ident       :: Integer
  , file        :: FilePath
  , include     :: FilePath
  , includeLib  :: FilePath
  , outputDir   :: FilePath
  , wait        :: TMVar () }

nextCount :: TVar Integer -> STM Integer
nextCount counter = do
    current <- readTVar counter
    writeTVar counter $! current + 1
    return current

setUpCompileServer :: IO ErlangCompiler
setUpCompileServer = do
    (Just stdinH, Just stdoutH, _err, _processHandle) <- createProcess processSpec
    hSetBuffering stdinH LineBuffering
    hSetBuffering stdoutH LineBuffering
    createCompiler stdinH stdoutH

createCompiler :: Handle -> Handle -> IO ErlangCompiler
createCompiler stdinH stdoutH = do
    resultMap <- atomically $ newTVar Map.empty
    chan <- atomically newTChan
    counter <- atomically $ newTVar 0
    forkIO undefined -- notifier thread
    return $ createRequestCompile chan counter

createRequestCompile :: TChan CompileRequest -> TVar Integer -> ErlangCompiler
createRequestCompile chan counter file include includeLib outputDir =
    liftIO $ do
        waitVar <- atomically $ do
            waitVar <- newEmptyTMVar
            let req = CompileRequest { ident = 10
                                     , file = file
                                     , include = include
                                     , includeLib = includeLib
                                     , outputDir = outputDir
                                     , wait = waitVar}
            writeTChan chan req
            return waitVar
        atomically $ takeTMVar waitVar

processSpec :: CreateProcess
processSpec = CreateProcess {
    cmdspec         = RawCommand "compile-server.escript" []
  , cwd             = Nothing
  , env             = Nothing
  , std_in          = CreatePipe
  , std_out         = CreatePipe
  , std_err         = CreatePipe
  , close_fds       = False
  , create_group    = False
  , delegate_ctlc   = False }


compileErlang :: ErlangCompiler
compileErlang file include includeLib outputDir =
    cmd "erlc" "-o" outputDir "-I" include "-I" includeLib file

runShake :: ErlangCompiler -> IO ()
runShake erlangCompiler = shakeArgs shakeOptions $ do

    phony "clean" $ do
        removeFilesAfter "ebin" ["//*"]

    "ebin/*.beam" *> \out -> do
        let src = "src" </> (dropDirectory1 $ out) -<.> "erl"
        need [src]
        erlangCompiler src "include" "deps" "ebin"

    "deps/*/ebin/*.beam" *> \out -> do
        let (dir, name) = splitFileName out
            src = (takeDirectory $ takeDirectory dir) </> "src" </> name -<.> "erl"
        need [src]
        let ebinDir = takeDirectory out
            includeDir = (takeDirectory $ takeDirectory out) </> "include"
        erlangCompiler src includeDir "deps" ebinDir

    "compile" ~> do
        cs <- getDirectoryFiles "src" ["*.erl"]
        depCs <- getDirectoryFiles "deps" ["*/src/*.erl"]
        let os = ["ebin" </> c -<.> "beam" | c <- cs]
            dos = ["deps" </> (takeDirectory1 d) </> "ebin" </> (dropDirectory1 $ dropDirectory1 d) -<.> "beam" | d <- depCs]
        need $ os ++ dos

main :: IO ()
main = runShake compileErlang
