{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.Archive.Tar (Entry,EntryContent(..))
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as Gzip
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Codec.Digest.SHA as Sha2
import           Data.List
import           Data.Serialize
import           Data.String
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Process

data Options = OptionSign FilePath | Verify FilePath

main = do
  (cmd:archive:_) <- getArgs
  case cmd of
    "sign" -> sumAndSign archive
    "verify" -> verify archive

sumAndSign :: FilePath -> IO ()
sumAndSign fp = do
  exists <- doesFileExist fp
  if not exists
     then error $ fp ++ " doesn't exist"
     else do gzip <- L.readFile fp
             entries <- getGzipEntries gzip
             L.writeFile sum (checksum entries)
             rawSystem "gpg" ["--detach-sign",sum]
             removeFile sum
             addSignature fp (sum <.> "sig") entries

  where sum = translate ".sum" fp

addSignature :: FilePath -> FilePath -> [Entry] -> IO ()
addSignature gz sig entries = do
  signature <- L.readFile sig
  case Tar.toTarPath False (makeSigName gz) of
    Left err -> error err
    Right spath -> do
      let sigEntry = Tar.fileEntry spath signature
      L.writeFile (translate ".signed.tar.gz" gz)
                  (Gzip.compress (Tar.write (sigEntry : entries)))
      removeFile sig

makeSigName = translate ".sig" . takeFileName

translate ext = (++ ext) . dropSigned . dropExtension . dropExtension where
  dropSigned x | isSuffixOf ".signed" x = dropExtension x
               | otherwise = x


getGzipEntries gzip =
  case result of
    Left err -> error ("tar reading error: " ++ show err)
    Right entries -> return entries

  where result = Tar.foldEntries (fmap . (:))
                                 (Right [])
                                 Left
                                 (Tar.read (Gzip.decompress gzip))

checksum :: [Entry] -> ByteString
checksum entries = L.intercalate "\n" (filter (not . L.null) (map hashEntry (sort (map Tar.entryContent entries))))

hashEntry :: EntryContent -> ByteString
hashEntry entry =
  case entry of
    NormalFile bytes _          -> hashHex bytes
    SymbolicLink target         -> encodeTarget target
    HardLink target             -> encodeTarget target
    OtherEntryType typ bytes _  -> hashHex (L.cons (word typ) bytes)
    CharacterDevice major minor -> L.cons (word 'c') (L.concat [iword major,iword minor])
    BlockDevice major minor     -> L.cons (word 'b') (L.concat [iword major,iword minor])
    _ -> L.empty

  where encodeTarget = fromString . Tar.fromLinkTarget
        iword = fromString . show
        word = fromIntegral . fromEnum
        hashHex = fromString . Sha2.showBSasHex . Sha2.hash Sha2.SHA256

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
                 L.writeFile sum (checksum (filter (not . isSig) entries))
                 L.writeFile sig (getEntryFileContent entry)
                 rawSystem "gpg" ["--verify",sig,sum]
                 removeFile sum
                 removeFile sig

  where sigName = makeSigName fp
        sum = translate ".sum" fp
        isSig = (==sigName) . Tar.entryPath
        sig = translate ".sig" fp

getEntryFileContent entry =
  case Tar.entryContent entry of
    NormalFile bytes _ -> bytes
    _ -> error "malformed signature in the tar archive"
