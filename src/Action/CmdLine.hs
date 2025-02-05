{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ApplicativeDo #-}

module Action.CmdLine(
    -- * Modes of execution
    Mode(..),
    SearchOpts(..),
    GenerateOpts(..),
    ServerOpts(..),
    ReplayOpts(..),
    TestOpts(..),
    -- * Endpoints
    ServerEndpoint(..),
    showEndpoint,
    -- * Parsing command line
    getCmdLine, defaultDatabaseLang,
    -- * Verbosity
    Verbosity,
    whenLoud, whenNormal
    ) where

import Data.List.Extra
import Data.Version
import General.Util
import Paths_hoogle (version)
import Options.Applicative as O
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Control.Monad
import Data.Maybe

data Verbosity = VerbosityNormal | VerbosityLoud
    deriving (Eq, Ord, Show)

whenLoud, whenNormal :: Verbosity -> IO () -> IO ()
whenLoud v k = when (v >= VerbosityLoud) k
whenNormal v k = when (v >= VerbosityNormal) k


data SearchOpts
    = SearchOpts
        { color :: Maybe Bool
        , json :: Bool
        , jsonl :: Bool
        , link :: Bool
        , numbers :: Bool
        , info :: Bool
        , database :: FilePath
        , count :: Maybe Int
        , query :: [String]
        , repeat_ :: Int
        , compare_ :: [String]
        }

data GenerateOpts
    = GenerateOpts
        { download :: Maybe Bool
        , database :: FilePath
        , insecure :: Bool
        , include :: [String]
        , count :: Maybe Int
        , local_ :: [FilePath]
        , haddock :: Maybe FilePath
        , debug :: Bool
        }

data ServerOpts
    = ServerOpts
        { endpoint :: ServerEndpoint
        , database :: FilePath
        , cdn :: String
        , logs :: FilePath
        , local :: Bool
        , haddock :: Maybe FilePath
        , links :: Bool
        , scope :: String
        , home :: String
        , https :: Bool
        , cert :: FilePath
        , key :: FilePath
        , datadir :: Maybe FilePath
        , no_security_headers :: Bool
        }

data ReplayOpts
    = ReplayOpts
        { logs :: FilePath
        , database :: FilePath
        , repeat_ :: Int
        , scope :: String
        }

data TestOpts
    = TestOpts
        { deep :: Bool
        , disable_network_tests  :: Bool
        , database :: FilePath
        }

data Mode
    = Search SearchOpts
    | Generate GenerateOpts
    | Server ServerOpts
    | Replay ReplayOpts
    | Test TestOpts

data ServerEndpoint
    = UnixSocket FilePath
    | TcpSocket String Int

showEndpoint :: ServerEndpoint -> String
showEndpoint (TcpSocket host port) = "port " <> show port <> " on host " <> host
showEndpoint (UnixSocket sock) = "socket " <> sock

defaultDatabaseLang :: IO FilePath
defaultDatabaseLang = do
    xdgLocation <- getXdgDirectory XdgData "hoogle"
    legacyLocation <- getAppUserDataDirectory "hoogle"
    doesXdgPathExist <- doesPathExist xdgLocation
    doesLegacyPathExist <- doesPathExist legacyLocation

    dir <- case (doesXdgPathExist, doesLegacyPathExist) of
      -- On Windows XDG location and legacy location are identical
      _ | xdgLocation == legacyLocation -> pure xdgLocation
      (_, False) -> pure xdgLocation
      (True, True) -> do
        hPutStrLn stderr $
          "Warning: Legacy location ignored (" ++ legacyLocation ++ "),"
          ++ "since xdg location is available (" ++ xdgLocation ++")."
        pure xdgLocation
      (False, True) -> do
        -- TODO: renable after release
        --hPutStrLn stderr $ "Warning: " ++ legacyLocation ++ " is deprecated."
        --  ++ "Consider moving it to $XDG_DATA_HOME/hoogle (" ++ xdgLocation ++ ")"
        pure legacyLocation
    pure $ dir </> "default-haskell-" ++ showVersion (trimVersion 3 version) ++ ".hoo"

-- N.B. This is rather awkward but seems to be the pragmatic way to migrate
-- away from cmdargs without changing the user-visible command-line syntax.
fillInDatabase :: FilePath -> Mode -> Mode
fillInDatabase defDb (Search opts)
  | "" <- opts.database = Search $ opts { database = defDb }
fillInDatabase defDb (Generate opts)
  | "" <- opts.database = Generate $ opts { database = defDb }
fillInDatabase defDb (Server opts)
  | "" <- opts.database = Server $ opts { database = defDb }
fillInDatabase defDb (Replay opts)
  | "" <- opts.database = Replay $ opts { database = defDb }
fillInDatabase defDb (Test opts)
  | "" <- opts.database = Test $ opts { database = defDb }
fillInDatabase _ mode = mode

getCmdLine :: [String] -> IO (Verbosity, Mode)
getCmdLine args = do
    (verbosity, mode) <- execParser cmdline

    -- fill in the default database TODO
    --args <- if args.database /= "" then pure args else do
    defDb <- defaultDatabaseLang
    pure (verbosity, fillInDatabase defDb mode)

cmdline :: ParserInfo (Verbosity, Mode)
cmdline =
    O.info ((,) <$> verbosity <*> mode' <**> helper <**> simpleVersioner (showVersion version)) (header name)
  where
    mode' = mode <|> fmap Search searchOpts
    verbosity = flag VerbosityNormal VerbosityLoud (short 'v' <> long "verbose" <> help "emit verbose output")
    name = "Hoogle " ++ showVersion version ++ ", https://hoogle.haskell.org/"

mode :: Parser Mode
mode = hsubparser
    $  command "search" (O.info (Search <$> searchOpts) (progDesc "Perform a search"))
    <> command "generate" (O.info (Generate <$> generateOpts) (progDesc "Generate Hoogle databases"))
    <> command "serve" (O.info (Server <$> serverOpts) (progDesc "Start a Hoogle server"))
    <> command "replay" (O.info (Replay <$> replayOpts) (progDesc "Replay a log file"))
    <> command "test" (O.info (Test <$> testOpts) (progDesc "Run the test suite"))

databaseFlag :: Parser FilePath
databaseFlag =
    option str (long "database" <> short 'd' <> metavar "FILE" <> help "Name of database to use (use .hoo extension)")

logsFlag :: Parser FilePath
logsFlag =
    option (fromMaybe "log.txt" <$> optional str) (value "" <> metavar "FILE" <> help "File to log requests to (defaults to stdout)")

repeatFlag :: Parser Int
repeatFlag =
    option auto (long "repeat" <> short 'r' <> value 1 <> help "Number of times to repeat (for benchmarking)")

scopeFlag :: Parser String
scopeFlag =
    option str (long "scope" <> short 's' <> help "Default scope to start with")

searchOpts :: Parser SearchOpts
searchOpts = do
    color <- optional $ switch (long "colour" <> help "Use colored output (requires ANSI terminal)")
    json <- switch (long "json" <> help "Get result as JSON")
    jsonl <- switch (long "jsonl" <> help "Get result as JSONL (JSON Lines)")
    link <- switch (long "link" <> help "Give URL's for each result")
    numbers <- switch (long "numbers" <> help "Give counter for each result")
    info <- switch (long "info" <> help "Give extended information about the first n results (set n with --count, default is 1)")
    database <- databaseFlag
    count <- optional $ option auto (short 'n' <> long "count" <> help "Maximum number of results to return (defaults to 10)")
    query <- some $ argument str (metavar "QUERY")
    repeat_ <- repeatFlag
    compare_ <- many $ option str (long "compare" <> metavar "SIG" <> help "Type signatures to compare against")
    pure $ SearchOpts {..}

generateOpts :: Parser GenerateOpts
generateOpts = do
    download <- optional $ switch (long "download" <> help "Download all files from the web")
    database <- databaseFlag
    insecure <- switch (long "insecure" <> short 'i' <> help "Allow insecure HTTPS connections")
    include <- many $ argument str (metavar "PACKAGE" <> help "Packages to include")
    local_ <- many $ option (fromMaybe "" <$> optional str) (long "local" <> short 'l' <> help "Index local packages and link to local haddock docs")
    count <- optional $ option auto (long "count" <> short 'n' <> help "Maximum number of packages to index (defaults to all)")
    haddock <- optional $ option str (long "haddock" <> short 'h' <> help "Use local haddocks")
    debug <- switch (long "debug" <> help "Generate debug information")
    pure $ GenerateOpts {..}

unixEndpoint :: Parser ServerEndpoint
unixEndpoint =
    UnixSocket <$> option str (long "socket" <> metavar "PATH" <> help "UNIX socket")

tcpEndpoint :: Parser ServerEndpoint
tcpEndpoint =
    TcpSocket <$> host <*> port
  where
    host = option str (long "host" <> value "*" <> help "Set the host to bind on (e.g., an ip address; '!4' for ipv4-only; '!6' for ipv6-only; default: '*' for any host).")
    port = option auto (long "port" <> short 'p' <> value 8080 <> metavar "PORT" <> help "Port number")

serverOpts :: Parser ServerOpts
serverOpts = do
    endpoint <- unixEndpoint <|> tcpEndpoint
    database <- databaseFlag
    cdn <- option str (value "" <> metavar "URL" <> help "URL prefix to use")
    logs <- logsFlag
    local <- switch (long "local" <> help "Allow following file:// links, restricts to 127.0.0.1  Set --host explicitely (including to '*' for any host) to override the localhost-only behaviour")
    haddock <- optional $ option str (long "haddock" <> metavar "DIR" <> help "Serve local haddocks from a specified directory")
    scope <- scopeFlag
    links <- switch (long "links" <> help "Display extra links")
    home <- option str (long "home" <> value "https://hoogle.haskell.org" <> metavar "URL" <> help "Set the URL linked to by the Hoogle logo.")
    https <- switch (long "https" <> help "Start an https server (use --cert and --key to specify paths to the .pem files)")
    cert <- option str (value "cert.pem" <> metavar "FILE" <> help "Path to the certificate pem file (when running an https server)")
    key <- option str (long "key" <> short 'k' <> value "key.pem" <> metavar "FILE" <> help "Path to the key pem file (when running an https server)")
    datadir <- optional $ option str (long "datadir" <> metavar "DIR" <> help "Override data directory path")
    no_security_headers <- switch (long "no-security-headers" <> short 'n' <> help "Don't send CSP security headers")
    pure ServerOpts {..}

replayOpts :: Parser ReplayOpts
replayOpts = do
    logs <- logsFlag
    database <- databaseFlag
    repeat_ <- repeatFlag
    scope <- scopeFlag
    pure ReplayOpts {..}

testOpts :: Parser TestOpts
testOpts = do
    deep <- switch (long "deep" <> help "Run extra long tests")
    database <- databaseFlag
    disable_network_tests <- switch (long "disable-network-tests" <> help "Disables the use of network tests")
    pure TestOpts {..}
