#!/usr/bin/env stack
{- stack
  --resolver lts-12.26 script
  --package directory
  --package filepath
  --package rio
  --package yaml -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

import RIO as P
import RIO.Process
import RIO.Text as T
import System.Directory as Dir
import System.FilePath
import System.Exit
import System.IO as IO

-- | For faster bootstarpping update together with --resolver in header ^
resolver :: String
resolver = "lts-12.26"

data Command
  = Install
  | Update
  deriving (Eq)

instance Show Command where
  show =
    \case
      Install -> "install"
      Update -> "update"

type Args = [String]

execInfo :: String -> Args -> RIO SimpleApp ()
execInfo cmd args = do
  logInfo $ display $ T.pack $ P.unwords (cmd : args)
  exitCode <- proc cmd args (runProcess . setStdin inherit)
  case exitCode of
    ExitSuccess -> return ()
    _ -> do
      logError $ "Executed process returned with: " <> displayShow exitCode
      liftIO $ exitWith exitCode

sudo :: Args -> RIO SimpleApp ()
sudo = execInfo "sudo"

aptGet :: Command -> Args -> RIO SimpleApp ()
aptGet cmd args = sudo $ ["apt-get", show cmd] <> args

stackInstall :: Args -> RIO SimpleApp ()
stackInstall args = execInfo "stack" $ ["--resolver", resolver, "install"] <> args

setupEmacs :: Int -> RIO SimpleApp ()
setupEmacs version
  | version < 24 || version > 26 = do
    logError $ "Unsupported emacs version: " <> displayShow version
  | otherwise = do
    hasKelleykPPA <-
      liftIO $ Dir.doesFileExist "/etc/apt/sources.list.d/kelleyk-ubuntu-emacs-bionic.list"
    unless hasKelleykPPA $ sudo ["add-apt-repository", "-y", "ppa:kelleyk/emacs"]
    aptGet Update ["-y"]
    let emacs = "emacs" <> show version
        pkgs = [emacs, emacs <> "-common", emacs <> "-el"]
    aptGet Install ("-y" : pkgs)
    homeDir <- liftIO Dir.getHomeDirectory
    let emacsHome = homeDir </> ".emacs.d"
        haskellDevEl = "haskell-dev.el"
        pathToHaskellDevEl = emacsHome </> haskellDevEl
    liftIO $ Dir.createDirectoryIfMissing True emacsHome
    logError $ displayShow emacsHome
    curDir <- liftIO Dir.getCurrentDirectory
    liftIO (Dir.doesFileExist pathToHaskellDevEl) >>= \alreadySetup ->
      unless alreadySetup $ do
        execInfo "ln" ["-s", curDir </> "conf" </> "haskell-dev.el", pathToHaskellDevEl]
        let loadCmd = "\n(load (expand-file-name (convert-standard-filename \"" <>
                      haskellDevEl <> "\") user-emacs-directory))"
        P.withFile (homeDir </> ".emacs") AppendMode $ \ h ->
          liftIO $ IO.hPutStrLn h loadCmd




main :: IO ()
main =
  runSimpleApp $ do
    let pkgs = ["git", "libtinfo-dev"]
    aptGet Install ("-y" : pkgs)
    stackInstall ["intero", "hindent", "hlint", "stylish-haskell"]
    setupEmacs 26
    -- TODO: add -fshow-loaded-modules to ghci as described in:
    -- https://github.com/haskell/haskell-mode/issues/1553#issuecomment-408929720
