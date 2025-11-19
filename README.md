# wx_gleam (WIP)

[![Package Version](https://img.shields.io/hexpm/v/wx_gleam)](https://hex.pm/packages/wx_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/wx_gleam/)

*(Note: The Hex Docs badge is kept for future use, but may not be functional yet.)*

A Gleam library for creating native GUIs using Erlang's wxWidgets (`wx`) bindings. This project provides a Gleam-friendly API by wrapping the underlying Erlang functions via the Foreign Function Interface (FFI).

The core logic is implemented in Erlang (`src/wx_gleam/internals/wx_ffi.erl`) and exposed to Gleam through FFI definitions (`src/wx_gleam/internals.gleam`).

## Features

- ✅ **Native GUI**: Uses wxWidgets for truly native look and feel on all platforms
- ✅ **Cross-Platform**: Works on Linux, macOS, and Windows
- ✅ **Type-Safe**: Leverages Gleam's type system for compile-time safety
- ✅ **Minimal Overhead**: Thin wrapper with minimal abstractions
- ✅ **Erlang Integration**: Seamlessly integrates with the BEAM ecosystem

## Current Limitations

- ⚠️ **Work in Progress**: This library is still in early development
- ⚠️ **Limited Widgets**: Currently supports basic frames and buttons only
- ⚠️ **No Event Handlers**: Button click events are not yet implemented
- ⚠️ **Not on Hex**: Must be used as a path dependency for now
- ⚠️ **Basic Layout**: No layout managers yet (widgets use default positioning)

Future development will expand widget support and add more wxWidgets functionality.

## Project Goals

- **Goal**: To be able to write code in Gleam that is equivalent to using Erlang's `wx` module.
- **Non-Goal**: Adding any unnecessary (or opinionated) abstractions in this library.

## Architecture

wx_gleam is designed as a thin Gleam wrapper around Erlang's `wx` module:

```
┌─────────────────────────────────────┐
│   Gleam Application Code            │
│   (Your application)                │
└───────────┬─────────────────────────┘
            │
┌───────────▼─────────────────────────┐
│   wx_gleam Public API               │
│   (src/wx_gleam.gleam)              │
│   - High-level, type-safe functions │
│   - Result types for error handling │
└───────────┬─────────────────────────┘
            │
┌───────────▼─────────────────────────┐
│   wx_gleam/internals FFI Bindings   │
│   (src/wx_gleam/internals.gleam)    │
│   - @external declarations          │
│   - Opaque types                    │
└───────────┬─────────────────────────┘
            │
┌───────────▼─────────────────────────┐
│   Erlang FFI Implementation         │
│   (src/wx_gleam/internals/wx_ffi.erl)│
│   - Type conversions                │
│   - Direct wx module calls          │
└───────────┬─────────────────────────┘
            │
┌───────────▼─────────────────────────┐
│   Erlang wx Module                  │
│   (Built into Erlang/OTP)           │
└───────────┬─────────────────────────┘
            │
┌───────────▼─────────────────────────┐
│   wxWidgets C++ Library             │
│   (Native GUI toolkit)              │
└─────────────────────────────────────┘
```

This layered architecture ensures:

- Type safety at the Gleam level
- Proper conversion between Gleam and Erlang types
- Direct access to wxWidgets functionality
- Minimal overhead

## Installation

**Note: This package is not yet published to Hex.**

To use it locally, you must add it as a path dependency in your `gleam.toml`, similar to the `example` project:

```toml
[dependencies]
gleam_stdlib = ">= 0.44.0 and < 2.0.0"
wx_gleam = { path = ".." }
```

## Usage

### Quick Start with `with_app`, `with_frame`, and `with_button`

The recommended way to use wx_gleam is with the `with_app`, `with_frame`, and `with_button` functions, which handle initialization and cleanup automatically:

```gleam
import wx_gleam

pub fn main() {
  // Use the with_app helper for automatic initialization and cleanup
  use wx_app <- wx_gleam.with_app()
  
  // Use the with_frame helper for automatic frame creation
  use frame <- wx_gleam.with_frame(wx_app, "Gleam WxApp")
  
  // Use the with_button helper for automatic button creation
  use _button <- wx_gleam.with_button(frame, "Click Me!")

  // Show the frame
  wx_gleam.show_frame(frame)

  // Connect the close event and wait for the close message
  wx_gleam.connect_close_event(frame)
  wx_gleam.await_close_message(fn(_) { Nil })
}
```

You can also mix the `use` syntax with explicit creation if needed:

```gleam
import wx_gleam

pub fn main() {
  use wx_app <- wx_gleam.with_app()
  use frame <- wx_gleam.with_frame(wx_app, "Gleam WxApp")

  // Create a button explicitly if you prefer
  let assert Ok(_button) = wx_gleam.create_button(frame, "Click Me!")

  wx_gleam.show_frame(frame)
  wx_gleam.connect_close_event(frame)
  wx_gleam.await_close_message(fn(_) { Nil })
}
```

You can also use `with_app` without `with_frame` if you prefer explicit frame creation:

```gleam
import wx_gleam

pub fn main() {
  use wx_app <- wx_gleam.with_app()

  let assert Ok(wx_frame) = wx_gleam.create_frame(wx_app, "Gleam WxApp")
  let assert Ok(_button) = wx_gleam.create_button(wx_frame, "Click Me!")

  wx_gleam.show_frame(wx_frame)
  wx_gleam.connect_close_event(wx_frame)
  wx_gleam.await_close_message(fn(_) { Nil })
}
```

### Manual Initialization

For more control over the application lifecycle, you can manually initialize and destroy the wx application:

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
  wx_gleam.await_close_message(fn(_) { Nil })

  // Clean up and destroy the wx application
  wx_gleam.destroy()
}
```

### API Overview

The wx_gleam library provides a simple, high-level API for common GUI operations:

- **`init_wx()`** - Initialize the wx application (returns `Result(WxApp, Dynamic)`)
- **`with_app()`** - Convenience wrapper that handles initialization and cleanup
- **`with_frame()`** - Convenience wrapper that handles frame creation using `use` syntax
- **`with_button()`** - Convenience wrapper that handles button creation using `use` syntax
- **`create_frame()`** - Create a new window with a title
- **`show_frame()`** - Make a window visible on screen
- **`create_button()`** - Create a button widget inside a frame
- **`connect_close_event()`** - Enable close event handling for a window
- **`await_close_message()`** - Wait for the user to close a window
- **`destroy()`** - Clean up and shut down the wx application

For more advanced functionality, you can use the lower-level FFI functions in the `wx_gleam/internals` module.

## Development

This project uses [Nix Flakes](https://nixos.wiki/wiki/Flakes) to provide a reproducible development environment with all necessary dependencies.

### Environment Setup

#### Option 1: Using Nix Flakes (Recommended)

If you have Nix with Flakes enabled:

```sh
nix develop
```

This will provide you with:

- Gleam 1.13.0
- Erlang/OTP 28.1.1
- All required build tools

#### Option 2: Using direnv

If you use `direnv` for automatic environment loading:

```sh
direnv allow
```

The environment will be loaded automatically when you enter the project directory.

#### Option 3: Manual Setup

If you prefer to install dependencies manually:

1. Install [Gleam](https://gleam.run/getting-started/installing/) (version 1.13.0 or later)
1. Install [Erlang/OTP](https://www.erlang.org/downloads) (version 28.1.1 or later)
1. Ensure wxWidgets development files are installed on your system

### Common Commands

- **Download Dependencies:**

  ```sh
  gleam deps download
  ```

  Downloads all dependencies specified in `gleam.toml` and `manifest.toml`.

- **Build the Project:**

  ```sh
  gleam build
  ```

  Compiles the Gleam and Erlang source files.

- **Run Tests:**

  ```sh
  gleam test
  ```

  Runs the test suite using [gleeunit](https://hexdocs.pm/gleeunit/).

- **Run the Example:**

  ```sh
  cd example
  gleam run
  ```

  Runs the example application that demonstrates basic wx_gleam usage.

- **Format Code:**

  This project uses `treefmt-nix` for formatting Gleam, Erlang, Nix, and other files.

  ```sh
  # Format all files (requires Nix)
  nix run .#treefmt
  ```

  Or, to format Gleam files only:

  ```sh
  gleam format
  ```

  Check formatting without making changes:

  ```sh
  gleam format --check src test
  ```

- **Generate Documentation:**

  ```sh
  gleam docs build
  ```

  Generates HTML documentation from doc comments. Open `build/dev/docs/index.html` to view.

### Continuous Integration

CI is configured using GitHub Actions. It runs tests and formatting checks on push and pull requests using Gleam `1.13.0` and OTP `28.1.1`.

See [`.github/workflows/test.yml`](.github/workflows/test.yml) for the complete CI configuration.

## Troubleshooting

### wxWidgets Not Found

If you get errors about wxWidgets not being found:

- **Linux**: Install wxWidgets development packages:

  ```sh
  # Ubuntu/Debian
  sudo apt-get install libwxgtk3.0-gtk3-dev

  # Fedora
  sudo dnf install wxGTK-devel
  ```

- **macOS**: wxWidgets is included with Erlang installed via Homebrew:

  ```sh
  brew install erlang
  ```

- **Nix**: The development environment includes wxWidgets automatically.

### Application Crashes on Startup

If the wx application crashes on startup, ensure:

1. You're running on a system with a GUI (X11, Wayland, macOS Aqua, or Windows)
1. The `DISPLAY` environment variable is set correctly (Linux/Unix)
1. Erlang's wx module is properly compiled with wxWidgets support

### Display Issues in Headless Environments

wx_gleam requires a graphical environment. For testing in headless environments (like CI), you may need to use a virtual display:

```sh
# Install Xvfb (X Virtual Framebuffer)
sudo apt-get install xvfb

# Run with virtual display
xvfb-run gleam run
```

## Contributing

Contributions are welcome! Here's how you can help:

### Reporting Issues

If you find a bug or have a feature request:

1. Check if an issue already exists in the [issue tracker](https://github.com/haruki7049/wx_gleam/issues)
1. If not, create a new issue with:
   - A clear description of the problem or feature
   - Steps to reproduce (for bugs)
   - Your environment details (OS, Gleam version, Erlang version)

### Submitting Changes

1. Fork the repository
1. Create a feature branch (`git checkout -b feature/amazing-feature`)
1. Make your changes following the project's style:
   - Add documentation comments for new functions
   - Follow existing code patterns
   - Keep changes minimal and focused
1. Format your code:
   ```sh
   gleam format
   ```
1. Run tests to ensure nothing breaks:
   ```sh
   gleam test
   ```
1. Commit your changes with clear messages
1. Push to your fork and create a pull request

### Development Guidelines

- **Documentation**: All public functions must have doc comments
- **Testing**: Add tests for new functionality when practical
- **Type Safety**: Leverage Gleam's type system for safety
- **Minimal Abstraction**: Follow the project goal of minimal abstractions
- **FFI Changes**: Be careful when modifying FFI code - test thoroughly

## See Also

### Related Documentation

- [Gleam Language Tour](https://tour.gleam.run/) - Learn Gleam basics
- [Erlang wx Documentation](https://www.erlang.org/doc/man/wx.html) - Official wx module docs
- [wxWidgets Documentation](https://docs.wxwidgets.org/) - wxWidgets C++ library docs
- [Gleam Erlang Target Guide](https://gleam.run/writing-gleam/gleam-erlang/) - Gleam-Erlang interop

### Similar Projects

- [wxErlang Examples](https://github.com/erlang/otp/tree/master/lib/wx/examples) - Official Erlang wx examples
- [Gleam Packages](https://packages.gleam.run/) - Discover other Gleam packages

## License

This project is licensed under the **MIT License**. See [LICENSE](LICENSE) for details.
