module Main (main) where

import Control.Monad (when)
import Data.List (find, nub)
import Data.Version (showVersion)
import qualified Paths_kewar as PathsKewar
import Kewar (CorrectionLevel (..), generate)
import Kewar.CLI.Grid (showG)
import System.Console.GetOpt (ArgDescr (NoArg, OptArg), ArgOrder (Permute), OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

data CLIException = InvalidErrorCorrectionLevel

instance Show CLIException where
  show InvalidErrorCorrectionLevel = "Provided error correction level is invalid. Supported options: L, M, Q, H\n"

data Options = Options
  { version :: Bool,
    help :: Bool,
    errorCorrection :: Either CLIException CorrectionLevel
  }

defaults :: Options
defaults =
  Options
    { version = False,
      help = False,
      errorCorrection = Right Q
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["version"] (NoArg parseVersion) "print qr version",
    Option ['h'] ["help"] (NoArg parseHelp) "print this help",
    Option ['e'] ["error-correction"] (OptArg parseErrorCorrection "LEVEL") "use error correction LEVEL for encoding. Defaults to Q"
  ]

-- | Parsers
parseHelp :: Options -> Options
parseHelp o = o {help = True}

parseVersion :: Options -> Options
parseVersion o = o {version = True}

parseErrorCorrection :: Maybe String -> Options -> Options
parseErrorCorrection maybeErrorCorrection o = case maybeErrorCorrection of
  Nothing -> o {errorCorrection = Right Q}
  Just ec -> case find (\j -> ec == show j) [H, Q, L, M] of
    Nothing -> o {errorCorrection = Left InvalidErrorCorrectionLevel}
    Just a -> o {errorCorrection = Right a}

parseCLIArguments :: [String] -> IO (Options, [String])
parseCLIArguments argv =
  case getOpt Permute options argv of
    (o, i, []) -> return (foldl (flip id) defaults o, i)
    (_, _, e) -> handleError e

-- | Handlers
handleVersion :: IO ()
handleVersion = do
  putStrLn $ "kewar-" ++ showVersion PathsKewar.version
  exitSuccess

handleHelp :: IO ()
handleHelp = do
  printHelp
  exitSuccess

handleError :: [String] -> IO a
handleError s = do
  putStrLn $ unlines $ map (("qr: error: " ++) . init) (nub s)
  printUsage
  exitFailure

-- | Utils
header :: String
header =
  unlines
    [ "kewar - (pronounced QR) generate QR code from given input string [version " ++ showVersion PathsKewar.version ++ "]",
      "",
      usage
    ]

usage :: String
usage = 
  unlines 
    [
      "Usage: kewar [options] [INPUT]",
      "",
      "kewar is a tool to generate QR codes from any supported string,",
      "utilizing an error correction level to allow data recovery.",
      "",
      "For more information on QR codes: https://www.qrcode.com/en/",
      "",
      "Options:"
    ]

printHelp :: IO ()
printHelp = putStrLn $ usageInfo header options

printUsage :: IO ()
printUsage = putStrLn $ usageInfo usage options

main :: IO ()
main = do
  args <- getArgs
  (opts, inputs) <- parseCLIArguments args

  when (help opts) handleHelp
  when (version opts) handleVersion

  case errorCorrection opts of
    Left e -> handleError [show e]
    Right ec -> case generate (head inputs) ec of
      Left e -> handleError [show e]
      Right g -> putStrLn $ showG g