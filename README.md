# wx_gleam (WIP)

[![Package Version](https://img.shields.io/hexpm/v/wx_gleam)](https://hex.pm/packages/wx_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/wx_gleam/)

*(Note: The Hex Docs badge is kept for future use, but may not be functional yet.)*

A Gleam library for creating native GUIs using Erlang's wxWidgets (`wx`) bindings. This project provides a Gleam-friendly API by wrapping the underlying Erlang functions via the Foreign Function Interface (FFI).

The core logic is implemented in Erlang (`src/wx_gleam/internals/wx_ffi.erl`) and exposed to Gleam through FFI definitions (`src/wx_gleam/internals.gleam`).

## Project Goals

- **Goal**: To be able to write code in Gleam that is equivalent to using Erlang's `wx` module.
- **Non-Goal**: Adding any unnecessary (or opinionated) abstractions in this library.

## Installation

**Note: This package is not yet published to Hex.**

To use it locally, you must add it as a path dependency in your `gleam.toml`, similar to the `example` project:

```toml
[dependencies]
gleam_stdlib = ">= 0.44.0 and < 2.0.0"
wx_gleam = { path = ".." }
```

## Usage

### Basic Example

Here is a basic example of how to initialize a `wx` application, create a window (Frame) with a button, and handle wx messages.

```gleam
import wx_gleam
import wx_gleam/wx_decode
import gleam/dynamic/decode

pub fn main() {
  use wx_app <- wx_gleam.with_app()

  // Create a frame (window)
  let assert Ok(wx_frame) = wx_gleam.create_frame(wx_app, "Gleam WxApp")

  // Create a button inside the frame
  let assert Ok(_button) = wx_gleam.create_button(wx_frame, "Click Me!")

  // Show the frame
  wx_gleam.show_frame(wx_frame)

  // Connect the close event and handle messages
  wx_gleam.connect_close_event(wx_frame)
  wx_gleam.apply_message_handler(message_handler)
}

fn message_handler(message: dynamic.Dynamic) -> Nil {
  case decode.run(message, wx_decode.wx_message()) {
    Ok(wx_msg) -> {
      case wx_msg.event {
        wx_gleam.WxClose(_) -> {
          // Handle close event
        }
        _ -> {
          // Handle other events
        }
      }
    }
    Error(_) -> {
      // Handle decode error
    }
  }
}
```

*\[Source: `example/src/example.gleam`\]*

### Message Types

The library now provides type-safe message handling with the following types:

- `WxMessage`: The main message type containing event information
- `WxEvent`: Various event types including:
  - `WxClose`: Window close events
  - `WxCommand`: Command/button events
  - `WxFocus`: Focus events
  - `WxKey`: Keyboard events
  - `WxMouse`: Mouse events
  - `WxSize`: Resize events
  - `WxUnknown`: Unknown or unsupported event types

Use the `wx_decode` module to decode dynamic messages into type-safe Gleam structures.

## Development

This project uses [Nix Flakes](https://nixos.wiki/wiki/Flakes) to provide a reproducible development environment.

### Environment Setup

1. If you have Nix with Flakes enabled, run `nix develop` to enter a shell with Gleam, Erlang, and other development tools.
1. Alternatively, if you use `direnv`, simply run `direnv allow`.

### Common Commands

- **Download Dependencies:**

  ```sh
  gleam deps download
  ```

  *\[Source: `.github/workflows/test.yml`\]*

- **Run Tests:**

  ```sh
  gleam test
  ```

  *\[Source: `.github/workflows/test.yml`\]*

- **Format Code:**
  This project uses `treefmt-nix` for formatting Gleam, Erlang, Nix, and other files.

  ```sh
  # Format all files
  nix run .#treefmt
  ```

  Or, to format Gleam files only:

  ```sh
  gleam format
  ```

  *\[Source: `.github/workflows/test.yml`\]*

### Continuous Integration

CI is configured using GitHub Actions. It runs tests and formatting checks on push and pull requests using Gleam `1.13.0` and OTP `28.1.1`.

## License

This project is licensed under the **MIT License**.
