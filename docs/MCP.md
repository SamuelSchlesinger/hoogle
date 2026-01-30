# Hoogle MCP Server

The Hoogle MCP (Model Context Protocol) server allows AI agents to search Haskell libraries programmatically. It exposes Hoogle's search functionality through the [MCP protocol](https://modelcontextprotocol.io/), enabling AI assistants like Claude to look up Haskell functions, types, and documentation.

## Installation

Build the MCP server along with Hoogle:

```bash
cabal build hoogle-mcp
```

Or install it:

```bash
cabal install hoogle-mcp
```

## Prerequisites

The MCP server requires a Hoogle database. Generate one before first use:

```bash
# Generate from Hackage/Stackage (recommended, but takes time)
hoogle generate

# Or generate from locally installed packages (faster)
hoogle generate --local
```

## Usage

### Running the Server

```bash
# Use default database location
hoogle-mcp

# Use a custom database
HOOGLE_DATABASE=/path/to/database.hoo hoogle-mcp
```

### Claude Code Configuration

Use the `claude mcp add` command to register the server:

```bash
# Add to current project (recommended - ties to project's resolver)
claude mcp add hoogle /path/to/hoogle-mcp

# Or add globally (available in all projects)
claude mcp add --scope user hoogle /path/to/hoogle-mcp

# With a custom database path
claude mcp add -e HOOGLE_DATABASE=/path/to/database.hoo hoogle /path/to/hoogle-mcp
```

Verify the server is connected:

```bash
claude mcp list
```

**Note:** Project-local configuration is recommended because Hoogle databases should match your project's package resolver. Generate a project-specific database to index only the packages you're using.

### Other MCP Clients

For other MCP-compatible clients, configure a stdio server with:
- **Command:** `hoogle-mcp` (or full path to the executable)
- **Environment:** `HOOGLE_DATABASE=/path/to/database.hoo` (optional)

## Available Tools

### `hoogle_search`

Search Haskell libraries by function name or type signature.

**Parameters:**
- `query` (required): The search query
- `count` (optional): Maximum results to return (default: 10, max: 100)

**Query Syntax:**
- Function name: `map`, `foldr`, `zip`
- Type signature: `a -> b`, `[a] -> Int`, `(a -> b) -> [a] -> [b]`
- Qualified name: `Data.List.map`, `Control.Monad.join`
- Package filter: `+base` (include only), `-containers` (exclude)
- Combined: `map +base`, `a -> b +transformers`

**Examples:**

```json
{"name": "hoogle_search", "arguments": {"query": "map"}}
{"name": "hoogle_search", "arguments": {"query": "a -> a", "count": 5}}
{"name": "hoogle_search", "arguments": {"query": "[a] -> Int +base"}}
{"name": "hoogle_search", "arguments": {"query": "Monad m => m a -> m b -> m b"}}
```

### `hoogle_info`

Get detailed information about a Haskell function or type.

**Parameters:**
- `query` (required): Function or type name (preferably qualified)

**Returns:**
- Complete type signature
- Package and module information
- Documentation/description
- Link to Hackage documentation

**Examples:**

```json
{"name": "hoogle_info", "arguments": {"query": "Data.List.map"}}
{"name": "hoogle_info", "arguments": {"query": "Control.Monad.join"}}
```

## Protocol Details

The MCP server communicates over stdio using JSON-RPC 2.0. It implements the MCP protocol version `2024-11-05`.

### Supported Methods

- `initialize` - Initialize the server
- `tools/list` - List available tools
- `tools/call` - Execute a tool
- `ping` - Health check

## Environment Variables

| Variable | Description |
|----------|-------------|
| `HOOGLE_DATABASE` | Path to the Hoogle database file (`.hoo`) |

## Troubleshooting

### "Failed to start MCP server: does not exist"

The Hoogle database file doesn't exist. Generate one:

```bash
hoogle generate
```

### "No results found"

- Check that your database is up to date: `hoogle generate`
- Try a simpler query to verify the database works: `map`
- Ensure the package containing your function is indexed

## Building a Custom Database

For project-specific documentation:

```bash
# From local Haddock files
hoogle generate --local=/path/to/docs --database=./project.hoo

# Use with MCP server
HOOGLE_DATABASE=./project.hoo hoogle-mcp
```

See the [Install documentation](./Install.md) for more database options.
