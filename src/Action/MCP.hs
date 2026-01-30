{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | MCP (Model Context Protocol) server for Hoogle
--
-- This module implements an MCP server that exposes Hoogle search
-- functionality to AI agents. It communicates over stdio using
-- JSON-RPC 2.0 as specified by the MCP protocol.
module Action.MCP
    ( actionMCP
    ) where

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout, stderr, hSetBuffering, BufferMode(..), stdin, hPutStrLn, isEOF)

import Action.CmdLine (defaultDatabaseLang, Language(..))
import Action.Search (search, targetInfo)
import General.Store (storeReadFile, StoreRead)
import Input.Item (Target(..))
import Query (Query, parseQuery)

-- | MCP Protocol version
mcpVersion :: Text
mcpVersion = "2024-11-05"

-- | Server information
serverName :: Text
serverName = "hoogle-mcp"

serverVersion :: Text
serverVersion = "0.1.0"

-- | JSON-RPC request structure
data JsonRpcRequest = JsonRpcRequest
    { reqJsonrpc :: Text
    , reqMethod :: Text
    , reqParams :: Maybe Value
    , reqId :: Maybe Value
    }

instance FromJSON JsonRpcRequest where
    parseJSON = withObject "JsonRpcRequest" $ \v -> JsonRpcRequest
        <$> v .: "jsonrpc"
        <*> v .: "method"
        <*> v .:? "params"
        <*> v .:? "id"

-- | JSON-RPC response structure
data JsonRpcResponse = JsonRpcResponse
    { respJsonrpc :: Text
    , respResult :: Maybe Value
    , respError :: Maybe JsonRpcError
    , respId :: Maybe Value
    }

instance ToJSON JsonRpcResponse where
    toJSON JsonRpcResponse{..} = object $ catMaybes
        [ Just ("jsonrpc" .= respJsonrpc)
        , ("result" .=) <$> respResult
        , ("error" .=) <$> respError
        , Just ("id" .= respId)
        ]

-- | JSON-RPC error structure
data JsonRpcError = JsonRpcError
    { errCode :: Int
    , errMessage :: Text
    , errData :: Maybe Value
    }

instance ToJSON JsonRpcError where
    toJSON JsonRpcError{..} = object $ catMaybes
        [ Just ("code" .= errCode)
        , Just ("message" .= errMessage)
        , ("data" .=) <$> errData
        ]

-- | MCP Tool definition
data MCPTool = MCPTool
    { toolName :: Text
    , toolDescription :: Text
    , toolInputSchema :: Value
    }

instance ToJSON MCPTool where
    toJSON MCPTool{..} = object
        [ "name" .= toolName
        , "description" .= toolDescription
        , "inputSchema" .= toolInputSchema
        ]

-- | MCP Content block for tool results
data MCPContent = MCPContent
    { contentType :: Text
    , contentText :: Text
    }

instance ToJSON MCPContent where
    toJSON MCPContent{..} = object
        [ "type" .= contentType
        , "text" .= contentText
        ]

-- | JSON-RPC error codes
errMethodNotFound, errInvalidParams :: Int
errMethodNotFound = -32601
errInvalidParams = -32602

-- | Run the MCP server - main entry point
--
-- The database path can be specified via the HOOGLE_DATABASE environment variable.
-- If not set, the default Hoogle database location is used.
actionMCP :: IO ()
actionMCP = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr LineBuffering

    -- Check for custom database path
    mCustomDb <- lookupEnv "HOOGLE_DATABASE"
    dbPath <- case mCustomDb of
        Just path -> pure path
        Nothing -> defaultDatabaseLang Haskell

    hPutStrLn stderr $ "Hoogle MCP server loading database: " ++ dbPath
    storeReadFile dbPath $ \db -> do
        hPutStrLn stderr "Database loaded successfully"
        processLines db
  `catch` \e -> do
    hPutStrLn stderr $ "Failed to start MCP server: " ++ show (e :: SomeException)
    hPutStrLn stderr ""
    hPutStrLn stderr "To fix this, either:"
    hPutStrLn stderr "  1. Generate a database: hoogle generate"
    hPutStrLn stderr "  2. Set HOOGLE_DATABASE environment variable to point to a .hoo file"

-- | Process incoming lines from stdin
processLines :: StoreRead -> IO ()
processLines db = do
    eof <- isEOF
    if eof
        then hPutStrLn stderr "EOF received, shutting down"
        else do
            line <- getLine
            when (not $ null line) $ do
                response <- handleRequest db line
                case response of
                    Just resp -> do
                        LBS8.putStrLn (encode resp)
                        hFlush stdout
                    Nothing -> pure ()
            processLines db

-- | Handle a single JSON-RPC request
handleRequest :: StoreRead -> String -> IO (Maybe JsonRpcResponse)
handleRequest db line = case eitherDecode (LBS8.pack line) of
    Left err -> pure $ Just $ errorResponse Nothing errInvalidParams
        (T.pack $ "Parse error: " ++ err)
    Right req -> handleJsonRpcRequest db req

-- | Handle a parsed JSON-RPC request
handleJsonRpcRequest :: StoreRead -> JsonRpcRequest -> IO (Maybe JsonRpcResponse)
handleJsonRpcRequest db req@JsonRpcRequest{..} = case reqMethod of
    "initialize" -> handleInitialize req
    "initialized" -> pure Nothing  -- Notification, no response
    "notifications/initialized" -> pure Nothing  -- Also a notification
    "tools/list" -> handleToolsList req
    "tools/call" -> handleToolsCall db req
    "ping" -> handlePing req
    _ -> pure $ Just $ errorResponse reqId errMethodNotFound
        (T.pack $ "Method not found: " ++ T.unpack reqMethod)

-- | Handle initialize request
handleInitialize :: JsonRpcRequest -> IO (Maybe JsonRpcResponse)
handleInitialize JsonRpcRequest{..} = pure $ Just $ JsonRpcResponse
    { respJsonrpc = "2.0"
    , respResult = Just $ object
        [ "protocolVersion" .= mcpVersion
        , "capabilities" .= object
            [ "tools" .= object []
            ]
        , "serverInfo" .= object
            [ "name" .= serverName
            , "version" .= serverVersion
            ]
        ]
    , respError = Nothing
    , respId = reqId
    }

-- | Handle ping request
handlePing :: JsonRpcRequest -> IO (Maybe JsonRpcResponse)
handlePing JsonRpcRequest{..} = pure $ Just $ JsonRpcResponse
    { respJsonrpc = "2.0"
    , respResult = Just $ object []
    , respError = Nothing
    , respId = reqId
    }

-- | Handle tools/list request
handleToolsList :: JsonRpcRequest -> IO (Maybe JsonRpcResponse)
handleToolsList JsonRpcRequest{..} = pure $ Just $ JsonRpcResponse
    { respJsonrpc = "2.0"
    , respResult = Just $ object
        [ "tools" .= availableTools
        ]
    , respError = Nothing
    , respId = reqId
    }

-- | Available MCP tools
availableTools :: [MCPTool]
availableTools =
    [ MCPTool
        { toolName = "hoogle_search"
        , toolDescription = T.unlines
            [ "Search Haskell libraries by function name or type signature."
            , ""
            , "Query syntax:"
            , "  - Function name: 'map', 'foldr', 'zip'"
            , "  - Type signature: 'a -> b', '[a] -> Int', '(a -> b) -> [a] -> [b]'"
            , "  - Qualified name: 'Data.List.map', 'Control.Monad.join'"
            , "  - Package filter: '+base' (include), '-containers' (exclude)"
            , "  - Combined: 'map +base', 'a -> b +transformers'"
            , ""
            , "Examples:"
            , "  - 'map' - find functions named 'map'"
            , "  - 'a -> a' - find identity-like functions"
            , "  - '[a] -> a' - find functions that extract from lists"
            , "  - 'Monad m => m a -> m b -> m b' - find monadic sequencing"
            ]
        , toolInputSchema = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "query" .= object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("The search query - function name, type signature, or combination" :: Text)
                    ]
                , "count" .= object
                    [ "type" .= ("integer" :: Text)
                    , "description" .= ("Maximum number of results (default: 10, max: 100)" :: Text)
                    ]
                ]
            , "required" .= (["query"] :: [Text])
            ]
        }
    , MCPTool
        { toolName = "hoogle_info"
        , toolDescription = T.unlines
            [ "Get detailed information about a Haskell function or type."
            , ""
            , "Returns full documentation including:"
            , "  - Complete type signature"
            , "  - Package and module information"
            , "  - Documentation/description"
            , "  - Link to Hackage documentation"
            , ""
            , "Use this after hoogle_search to get more details about a specific result."
            , "For best results, use a qualified name like 'Data.List.map'."
            ]
        , toolInputSchema = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "query" .= object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Function or type name to look up (preferably qualified)" :: Text)
                    ]
                ]
            , "required" .= (["query"] :: [Text])
            ]
        }
    ]

-- | Handle tools/call request
handleToolsCall :: StoreRead -> JsonRpcRequest -> IO (Maybe JsonRpcResponse)
handleToolsCall db JsonRpcRequest{..} = do
    let params = fromMaybe (Object mempty) reqParams
    case params of
        Object obj -> case (KM.lookup "name" obj, KM.lookup "arguments" obj) of
            (Just (String name), Just (Object args)) ->
                callTool db reqId name args
            (Just (String name), Nothing) ->
                callTool db reqId name mempty
            _ -> pure $ Just $ errorResponse reqId errInvalidParams
                "Missing or invalid 'name' parameter"
        _ -> pure $ Just $ errorResponse reqId errInvalidParams
            "Invalid params object"

-- | Call a specific tool
callTool :: StoreRead -> Maybe Value -> Text -> Object -> IO (Maybe JsonRpcResponse)
callTool db reqId name args = case name of
    "hoogle_search" -> handleHoogleSearch db reqId args
    "hoogle_info" -> handleHoogleInfo db reqId args
    _ -> pure $ Just $ errorResponse reqId errMethodNotFound
        (T.pack $ "Unknown tool: " ++ T.unpack name)

-- | Handle hoogle_search tool call
handleHoogleSearch :: StoreRead -> Maybe Value -> Object -> IO (Maybe JsonRpcResponse)
handleHoogleSearch db reqId args = do
    let mQuery = case KM.lookup "query" args of
            Just (String q) -> Just (T.unpack q)
            _ -> Nothing
        count = case KM.lookup "count" args of
            Just (Number n) -> min 100 $ max 1 $ round n
            _ -> 10

    case mQuery of
        Nothing -> pure $ Just $ errorResponse reqId errInvalidParams
            "Missing required 'query' parameter"
        Just query -> do
            let queries = parseQuery query
                (_, results) = search db queries
                formatted = formatSearchResults (take count results)
            pure $ Just $ successResponse reqId
                [ MCPContent "text" (T.pack formatted) ]

-- | Handle hoogle_info tool call
handleHoogleInfo :: StoreRead -> Maybe Value -> Object -> IO (Maybe JsonRpcResponse)
handleHoogleInfo db reqId args = do
    let mQuery = case KM.lookup "query" args of
            Just (String q) -> Just (T.unpack q)
            _ -> Nothing

    case mQuery of
        Nothing -> pure $ Just $ errorResponse reqId errInvalidParams
            "Missing required 'query' parameter"
        Just query -> do
            let queries = parseQuery query
                (_, results) = search db queries
            case take 1 results of
                [] -> pure $ Just $ successResponse reqId
                    [ MCPContent "text" "No results found for this query." ]
                (target:_) -> do
                    let info = formatTargetInfo queries target
                    pure $ Just $ successResponse reqId
                        [ MCPContent "text" (T.pack info) ]

-- | Format search results for display
formatSearchResults :: [Target] -> String
formatSearchResults targets
    | null targets = "No results found."
    | otherwise = unlines $ zipWith formatResult [1..] targets
  where
    formatResult :: Int -> Target -> String
    formatResult n target = unlines
        [ show n ++ ". " ++ signature ++ location
        , "   " ++ targetURL target
        , if null docPreview then "" else "   " ++ docPreview
        ]
      where
        signature = stripHtml (targetItem target)
        location = maybe "" (\(pkg, _) -> "  [" ++ pkg ++ "]") (targetPackage target)
        docs = stripHtml $ targetDocs target
        docPreview = if null docs then "" else take 150 docs ++ if length docs > 150 then "..." else ""

-- | Format detailed target information
formatTargetInfo :: [Query] -> Target -> String
formatTargetInfo queries target = targetInfo False queries target

-- | Strip HTML tags from a string
stripHtml :: String -> String
stripHtml = unwords . words . go False
  where
    go _ [] = []
    go inTag (c:cs)
        | c == '<' = go True cs
        | c == '>' = ' ' : go False cs
        | inTag = go inTag cs
        | otherwise = c : go inTag cs

-- | Create a success response with content
successResponse :: Maybe Value -> [MCPContent] -> JsonRpcResponse
successResponse reqId content = JsonRpcResponse
    { respJsonrpc = "2.0"
    , respResult = Just $ object
        [ "content" .= content
        ]
    , respError = Nothing
    , respId = reqId
    }

-- | Create an error response
errorResponse :: Maybe Value -> Int -> Text -> JsonRpcResponse
errorResponse reqId code message = JsonRpcResponse
    { respJsonrpc = "2.0"
    , respResult = Nothing
    , respError = Just $ JsonRpcError code message Nothing
    , respId = reqId
    }
