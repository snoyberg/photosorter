module Import.Fay
    ( fayFile
    ) where

import Prelude
import Yesod
import Language.Haskell.TH.Syntax
import Language.Fay.Compiler
import Language.Fay.Types
import Data.Default (def)
import Text.Julius
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import Settings.Development (development)
import System.Process (rawSystem)
import System.Exit (ExitCode (ExitSuccess))
import Control.Monad (unless)

fayFile :: String -> Q Exp
fayFile
    | development = fayFileReload
    | otherwise   = fayFileProd

fayFileProd :: String -> Q Exp
fayFileProd name = do
    qAddDependentFile fp
    ec <- qRunIO $ rawSystem "ghc" ["-O0", "--make", "-ifay", "-ifay-shared", fp]
    unless (ec == ExitSuccess) $ error $ "Type checking of fay module failed: " ++ name
    eres <- qRunIO $ compileFile config fp
    case eres of
        Left e -> error $ "Unable to compile Fay module \"" ++ name ++ "\": " ++ show e
        Right s -> [|toWidget $ const $ Javascript $ fromText $ T.pack s|]
  where
    fp = mkfp name

mkfp :: String -> FilePath
mkfp name = "fay/" ++ name ++ ".hs"

config :: CompileConfig
config = def
    { configDirectoryIncludes = ["fay", "fay-shared"]
    }

fayFileReload :: String -> Q Exp
fayFileReload name = [|
    liftIO (compileFile config $ mkfp name) >>= \eres ->
    (case eres of
        Left e -> error $ "Unable to compile Fay module \"" ++ name ++ "\": " ++ show e
        Right s -> toWidget $ const $ Javascript $ fromText $ T.pack s)|]
