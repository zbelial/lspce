# LSPCE Rust API Documentation

This document describes the Emacs API functions exposed by the LSPCE Rust module.

## Logging Functions

### `change_max_diagnostics_count(count: i32) -> String`
Changes the maximum number of diagnostics to cache. Returns a success message.

### `read_max_diagnostics_count() -> i32`
Returns the current maximum number of diagnostics being cached.

### `disable_logging() -> String`
Disables logging to /tmp/lspce.log. Returns a success message.

### `enable_logging() -> String`
Enables logging to /tmp/lspce.log. Returns a success message.

### `set_log_level(level: u8) -> String`
Sets the logging level. Returns a success message.

### `get_log_level() -> u8`
Returns the current logging level.

### `set_log_file(file: String) -> String`
Sets the logging file path. Returns a success message.

## Server Management Functions

### `connect(root_uri: String, lsp_type: String, cmd: String, cmd_args: String, initialize_req: String, timeout: i32, emacs_envs: String) -> Option<String>`
Connects to an LSP server or creates a new server process. Returns server info as JSON string if successful.

### `shutdown(root_uri: String, file_type: String, req: String) -> Option<String>`
Shuts down an LSP server. Returns None.

### `server(root_uri: String, file_type: String) -> Option<String>`
Returns server info as JSON string if server exists.

## Request/Response Functions

### `request_async(root_uri: String, file_type: String, req: String) -> Option<bool>`
Sends an async request to the LSP server. Returns true if successful.

### `notify(root_uri: String, file_type: String, req: String) -> Option<bool>`
Sends a notification to the LSP server. Returns true if successful.

### `read_response_exact(root_uri: String, file_type: String, id: String, method: String) -> Option<String>`
Reads a specific response by request ID. Returns response content as JSON string.

### `read_notification(root_uri: String, file_type: String) -> Option<String>`
Reads the next notification from the server. Returns notification content as JSON string.

## Diagnostic Functions

### `read_file_diagnostics(root_uri: String, file_type: String, uri: String) -> Option<String>`
Reads diagnostics for a specific file URI. Returns diagnostics as JSON string.

### `read_latest_response_id(root_uri: String, file_type: String) -> Option<String>`
Returns the ID of the latest response received.

### `read_latest_response_tick(root_uri: String, file_type: String) -> Option<String>`
Returns the tick value of the latest response received.
