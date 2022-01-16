module Main where

import QR (CorrectionLevel (..), generate, showG)

import Data.Either (fromRight)
import Data.List (find, nub)
import Data.Maybe (Maybe (Nothing), fromMaybe)
import Data.Version (showVersion)
import GHC.IO.Exception (IOErrorType (InvalidArgument))
import qualified Paths_qr as PathsQR
import System.Console.GetOpt (ArgDescr (NoArg, OptArg, ReqArg), ArgOrder (Permute), OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs)
import System.Exit (die, exitFailure, exitSuccess)
import System.IO.Error (mkIOError)
import Control.Monad (when)
import Data.Data (gcast1)

data CLIException = InvalidErrorCorrectionLevel | InvalidInput deriving (Eq)

instance Show CLIException where
  show InvalidErrorCorrectionLevel = "Provided error correction level is invalid. Supported options: L, M, Q, H"

data Options = Options
  { version :: Bool,
    help :: Bool,
    errorCorrection :: Either CLIException CorrectionLevel
  }

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
parseHelp o = o {help = True}

parseVersion o = o {version = True}

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

header :: String
header =
  unlines
    [ "qr - generate QR code from given input [version " ++ showVersion PathsQR.version ++ "]",
      "",
      "Usage: qr [options] [INPUT]",
      "",
      "qr is a tool to generate QR codes from any supported string,",
      "utilizing an error correction level to allow data recovery.",
      "",
      "For more information on QR codes: https://www.qrcode.com/en/",
      "",
      "Usage:"
    ]

handleVersion = do
  putStrLn $ "qr-" ++ showVersion PathsQR.version
  exitSuccess

handleHelp = do
  printHelp
  exitSuccess

handleError s = do
  putStrLn $ unlines $ map (("qr: error: " ++) . init) (nub s)
  printHelp
  exitFailure

printHelp = putStrLn $ usageInfo header options

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