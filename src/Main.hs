{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main where

import           Codec.Archive.Tar (Entry,EntryContent(..))
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as S
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.List
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    (cmd:archive:_) -> run cmd archive
    [archive] -> run "sign" archive
    _ -> help

run :: String -> FilePath -> IO ()
run cmd archive =
  case cmd of
    "sign" -> sumAndSign archive
    "verify" -> verify archive
    _ -> help

help :: IO ()
help = error "arguments: <sign|verify> <archive>"

sumAndSign :: FilePath -> IO ()
sumAndSign fp = do
  exists <- doesFileExist fp
  if not exists
     then error $ fp ++ " doesn't exist"
     else do gzip <- fmap (L.fromChunks . return) (S.readFile fp)
             entries <- getGzipEntries gzip
             L.writeFile tar (Gzip.decompress gzip)
             _ <- rawSystem "gpg" ["--detach-sign",tar]
             addSignature fp (tar <.> "sig") entries
             removeFile tar

  where tar = translate ".tar" fp

addSignature :: FilePath -> FilePath -> [Entry] -> IO ()
addSignature gz sig entries = do
  signature <- L.readFile sig
  case Tar.toTarPath False (makeSigName gz) of
    Left err -> error err
    Right spath -> do
      let sigEntry = Tar.fileEntry spath signature
      L.writeFile gz (Gzip.compress (Tar.write (sigEntry : entries)))
      removeFile sig

projectName :: FilePath -> [Char]
projectName = dropWhile (=='-') . translate "" . takeFileName

makeSigName :: FilePath -> FilePath
makeSigName gz = projectName gz </> filename gz where
  filename = translate ".sig" . reverse . drop 1 . dropWhile (/='-') . reverse . takeFileName

translate :: [Char] -> FilePath -> [Char]
translate ext = (++ ext) . dropSigned . dropExtension . dropExtension where
  dropSigned x | isSuffixOf ".signed" x = dropExtension x
               | otherwise = x


getGzipEntries :: ByteString -> IO [Entry]
getGzipEntries gzip =
  case result of
    Left err -> error ("tar reading error: " ++ show err)
    Right entries -> return entries

  where result = Tar.foldEntries (\entry -> either Left (Right . (entry:)))
                                 (Right [])
                                 Left
                                 (Tar.read (Gzip.decompress gzip))

verify :: FilePath -> IO ()
verify fp = do
  exists <- doesFileExist fp
  if not exists
     then error $ fp ++ " doesn't exist"
     else do gzip <- L.readFile fp
             entries <- getGzipEntries gzip
             case find isSig entries of
               Nothing -> error $ "unable to find " ++ sigName ++ " in archive"
               Just entry -> do
                 L.writeFile stripped (Tar.write (filter (not . isSig) entries))
                 L.writeFile sig (getEntryFileContent entry)
                 _ <- rawSystem "gpg" ["--verify",sig,stripped]
                 removeFile stripped
                 removeFile sig

  where sigName = makeSigName fp
        stripped = translate ".stripped" fp
        isSig = (==sigName) . Tar.entryPath
        sig = translate ".sig" fp

getEntryFileContent :: Entry -> ByteString
getEntryFileContent entry =
  case Tar.entryContent entry of
    NormalFile bytes _ -> bytes
    _ -> error "malformed signature in the tar archive"
