{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, takeTMVar, putTMVar)
import System.IO (Handle, BufferMode(LineBuffering), hSetBuffering, hPrint, hGetLine)
import qualified System.Process as Process
import Development.Shake (Action, shakeArgs, shakeOptions, (*>), (~>), phony,
                          removeFilesAfter, want, need, getDirectoryFiles,
                          traced, cmd)
import Development.Shake.FilePath ((</>), (-<.>), takeDirectory, takeDirectory1,
                                   dropDirectory1, splitFileName)

type ErlangCompiler = FilePath -> FilePath -> FilePath -> FilePath -> Action ()

data CompileRequest = CompileRequest {
    ident       :: Integer
  , file        :: FilePath
  , include     :: FilePath
  , includeLib  :: FilePath
  , outputDir   :: FilePath
  , wait        :: TMVar () }

instance Show CompileRequest where
    show CompileRequest {..} =
       unwords [show ident, file, include, includeLib, outputDir]

nextCount :: TVar Integer -> STM Integer
nextCount counter = do
    current <- readTVar counter
    writeTVar counter $! current + 1
    return current

setUpCompileServer :: IO ErlangCompiler
setUpCompileServer = do
    (Just stdinH, Just stdoutH, _err, _processHandle) <- Process.createProcess processSpec
    hSetBuffering stdinH LineBuffering
    hSetBuffering stdoutH LineBuffering
    createCompiler stdinH stdoutH

createCompiler :: Handle -> Handle -> IO ErlangCompiler
createCompiler stdinH stdoutH = do
    resultMap <- atomically $ newTVar Map.empty
    chan <- atomically newTChan
    counter <- atomically $ newTVar 0
    _ <- forkIO $ requestLoop stdinH chan resultMap
    _ <- forkIO $ listenerLoop stdoutH resultMap
    return $ createRequestCompile chan counter

requestLoop :: Handle -> TChan CompileRequest -> TVar (Map.Map Integer (TMVar ())) -> IO ()
requestLoop stdinH chan resultMap = do
    req <- atomically $ do
        req@ CompileRequest{ident = k, wait = v} <- readTChan chan
        modifyTVar resultMap (Map.insert k v)
        return req
    hPrint stdinH req
    requestLoop stdinH chan resultMap

listenerLoop :: Handle -> TVar (Map.Map Integer (TMVar ())) -> IO ()
listenerLoop stdoutH resultMap = do
    line <- hGetLine stdoutH
    let compileId = read line :: Integer
    atomically $ do
        m <- readTVar resultMap
        let (Just notifier) = Map.lookup compileId m
        putTMVar notifier ()
        -- TODO: clear the mapping
    listenerLoop stdoutH resultMap

createRequestCompile :: TChan CompileRequest -> TVar Integer -> ErlangCompiler
createRequestCompile chan counter file include includeLib outputDir =
    traced "erlc" $ do
        waitVar <- atomically $ do
            waitVar <- newEmptyTMVar
            count <- nextCount counter
            let req = CompileRequest { ident = count
                                     , file = file
                                     , include = include
                                     , includeLib = includeLib
                                     , outputDir = outputDir
                                     , wait = waitVar}
            writeTChan chan req
            return waitVar
        atomically $ takeTMVar waitVar

processSpec :: Process.CreateProcess
processSpec = Process.CreateProcess {
    cmdspec         = Process.RawCommand "compile-server.escript" []
  , cwd             = Nothing
  , env             = Nothing
  , std_in          = Process.CreatePipe
  , std_out         = Process.CreatePipe
  , std_err         = Process.Inherit
  , close_fds       = False
  , create_group    = False
  , delegate_ctlc   = False }


compileErlang :: ErlangCompiler
compileErlang file include includeLib outputDir =
    cmd "erlc" "-o" outputDir "-I" include "-I" includeLib file

runShake :: ErlangCompiler -> IO ()
runShake erlangCompiler = shakeArgs shakeOptions $ do
    want ["compile"]

    phony "clean" $
        removeFilesAfter "ebin" ["//*"]

    "ebin/*.beam" *> \out -> do
        let src = "src" </> dropDirectory1 out -<.> "erl"
        need [src]
        erlangCompiler src "include" "deps" "ebin"

    "deps/*/ebin/*.beam" *> \out -> do
        let (dir, name) = splitFileName out
            src = takeDirectory (takeDirectory dir) </> "src" </> name -<.> "erl"
        need [src]
        let ebinDir = takeDirectory out
            includeDir = takeDirectory (takeDirectory out) </> "include"
        erlangCompiler src includeDir "deps" ebinDir

    "compile" ~> do
        cs <- getDirectoryFiles "src" ["*.erl"]
        depCs <- getDirectoryFiles "deps" ["*/src/*.erl"]
        let os = ["ebin" </> c -<.> "beam" | c <- cs]
            dos = ["deps" </> takeDirectory1 d </> "ebin" </> dropDirectory1 (dropDirectory1 d) -<.> "beam" | d <- depCs]
        need $ os ++ dos

main :: IO ()
main = setUpCompileServer >>= runShake
