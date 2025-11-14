# wx_gleam (WIP)

-[![Package Version](https://img.shields.io/hexpm/v/wx_gleam)](https://hex.pm/packages/wx_gleam)
-[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/wx_gleam/)
*(Note: The Hex Docs badge is kept for future use, but may not be functional yet.)*

A Gleam library for creating native GUIs using Erlang's wxWidgets (`wx`) bindings. This project provides a Gleam-friendly API by wrapping the underlying Erlang functions via the Foreign Function Interface (FFI).

The core logic is implemented in Erlang (`src/wx_gleam/internals/wx_ffi.erl`) and exposed to Gleam through FFI definitions (`src/wx_gleam/internals.gleam`).

## Installation

**Note: This package is not yet published to Hex.**

To use it locally, you must add it as a path dependency in your `gleam.toml`, similar to the `example` project:

```toml
[dependencies]
gleam_stdlib = ">= 0.44.0 and < 2.0.0"
wx_gleam = { path = ".." }
```

## Usage

Here is a basic example of how to initialize a `wx` application, create a window (Frame) with a button, and wait for the window to be closed.

```gleam
import wx_gleam

pub fn main() {
  // Initialize the wx application
  let assert Ok(wx_app) = wx_gleam.init_wx()

  // Create a frame (window)
  let assert Ok(wx_frame) = wx_gleam.create_frame(wx_app, "Gleam WxApp")

  // Create a button inside the frame
  let assert Ok(_button) = wx_gleam.create_button(wx_frame, "Click Me!")

  // Show the frame
  wx_gleam.show_frame(wx_frame)

  // Connect the close event and wait for the close message
  wx_gleam.connect_close_event(wx_frame)
  wx_gleam.await_close_message()

  // Clean up and destroy the wx application
  wx_gleam.destroy()
}
```

*\[Source: `example/src/example.gleam`\]*

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
