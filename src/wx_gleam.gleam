//// A Gleam library for creating native GUIs using Erlang's wxWidgets (wx) bindings.
////
//// This module provides a Gleam-friendly API by wrapping the underlying Erlang
//// functions via the Foreign Function Interface (FFI). The core logic is
//// implemented in Erlang (`src/wx_gleam/internals/wx_ffi.erl`) and exposed to 
//// Gleam through FFI definitions (`src/wx_gleam/internals.gleam`).
////
//// ## Project Goals
////
//// - **Goal**: To be able to write code in Gleam that is equivalent to using 
////   Erlang's `wx` module.
//// - **Non-Goal**: Adding any unnecessary (or opinionated) abstractions in 
////   this library.
////
//// ## Basic Usage
////
//// The simplest way to use wx_gleam is with the `with_app` function, which
//// handles initialization and cleanup automatically. You can also combine it
//// with `with_frame` and `with_button` for cleaner resource management:
////
//// ```gleam
//// import wx_gleam
////
//// pub fn main() {
////   use wx_app <- wx_gleam.with_app()
////   use frame <- wx_gleam.with_frame(wx_app, "My App")
////   use _button <- wx_gleam.with_button(frame, "Click Me!")
////   
////   wx_gleam.show_frame(frame)
////   wx_gleam.connect_close_event(frame)
////   wx_gleam.await_close_message(fn(_) { Nil })
//// }
//// ```
////
//// ## Manual Initialization
////
//// For more control, you can manually initialize and destroy the wx application:
////
//// ```gleam
//// import wx_gleam
////
//// pub fn main() {
////   let assert Ok(wx_app) = wx_gleam.init_wx()
////   let assert Ok(wx_frame) = wx_gleam.create_frame(wx_app, "My App")
////   let assert Ok(_button) = wx_gleam.create_button(wx_frame, "Click Me!")
////   
////   wx_gleam.show_frame(wx_frame)
////   wx_gleam.connect_close_event(wx_frame)
////   wx_gleam.await_close_message(fn(_) { Nil })
////   wx_gleam.destroy()
//// }
//// ```
////
//// ## Error Handling
////
//// Most functions that can fail return a `Result` type. The `init_wx` and
//// `create_frame` functions return errors as `dynamic.Dynamic` for flexibility,
//// while convenience functions like `create_button` return `Error(Nil)` for
//// simplicity.

import gleam/dynamic
import gleam/result
import wx_gleam/internals

// --- Type definitions ---

/// Represents a wxWidgets application instance.
///
/// This is the main entry point for any wxWidgets application and must be
/// initialized before creating any GUI components. The WxApp type is an opaque
/// reference to the underlying Erlang wx server process.
///
/// ## Lifecycle
///
/// 1. Create: Obtain via `init_wx()` or `with_app()`
/// 2. Use: Pass to functions like `create_frame()` to create GUI components
/// 3. Destroy: Clean up with `destroy()` (automatic with `with_app()`)
pub type WxApp =
  internals.WxApp

/// Represents a wxWidgets frame (window).
///
/// A frame is a top-level window that can contain other GUI components like
/// buttons, text fields, panels, menus, and toolbars. It typically has a title
/// bar, borders, and system-provided minimize/maximize/close buttons.
///
/// ## Properties
///
/// - Default size: 400x300 pixels (configurable in the FFI layer)
/// - Initially hidden (call `show_frame()` to make it visible)
/// - Can be used as a parent for child widgets
/// - Supports event handling (e.g., close events)
pub type WxFrame =
  internals.WxFrame

/// Represents a wxWidgets button control.
///
/// A button is a GUI control that can be clicked by the user to trigger actions.
/// Buttons display text labels and respond to mouse clicks and keyboard activation.
///
/// ## Properties
///
/// - Displays a text label specified at creation time
/// - Can be clicked with mouse or activated with keyboard (Space/Enter)
/// - Must be a child of a frame or other container widget
/// - Automatically assigned an ID if created with `create_button()`
pub type WxButton =
  internals.WxButton

// --- Constants ---

/// A constant representing wxID_ANY (-1).
///
/// This special ID value tells wxWidgets to automatically assign a unique ID to
/// a widget. Using this constant is recommended for most widgets unless you need
/// to reference them by a specific ID for event handling or other purposes.
///
/// ## Usage
///
/// This constant is used internally by convenience functions like `create_button()`.
/// When working with the lower-level FFI functions directly, you can pass this
/// value as the ID parameter.
pub const id_any = -1

/// Initializes a wxWidgets application.
///
/// This function must be called before creating any GUI components.
/// It starts the wx Erlang application and creates a new wx server instance.
///
/// ## Important Notes
///
/// - This function should only be called once per application lifecycle
/// - All GUI components must be created after this initialization
/// - Consider using `with_app` for automatic initialization and cleanup
///
/// ## Returns
///
/// - `Ok(WxApp)` - The initialized application instance
/// - `Error(dynamic.Dynamic)` - An error if initialization fails (e.g., if wx
///   is already running or if the wx application cannot be started)
///
/// ## Example
///
/// ```gleam
/// let assert Ok(wx_app) = init_wx()
/// // Create GUI components using wx_app
/// // ...
/// destroy()
/// ```
pub fn init_wx() -> Result(WxApp, dynamic.Dynamic) {
  internals.init_wx()
}

/// A convenience function that initializes the wx application, runs a mainloop,
/// and cleans up automatically.
///
/// This is the recommended way to use wx_gleam as it ensures proper resource
/// cleanup even if errors occur. The function handles the boilerplate of 
/// initializing wx, running your application logic, and destroying the wx
/// instance when done.
///
/// ## Parameters
///
/// - `mainloop` - A function that receives the WxApp instance and contains
///   your application logic. This function will be executed after wx is
///   initialized and before cleanup occurs.
///
/// ## Behavior
///
/// 1. Initializes the wx application (asserts success)
/// 2. Executes your mainloop function with the WxApp instance
/// 3. Automatically calls `destroy()` to clean up resources
///
/// ## Example
///
/// ```gleam
/// use wx_app <- wx_gleam.with_app()
/// let assert Ok(frame) = create_frame(wx_app, "My Window")
/// show_frame(frame)
/// connect_close_event(frame)
/// await_close_message(fn(_) { Nil })
/// ```
///
/// ## Note
///
/// This function will panic if wx initialization fails. If you need to handle
/// initialization errors, use `init_wx()` and `destroy()` manually.
pub fn with_app(mainloop: fn(WxApp) -> Nil) -> Nil {
  let assert Ok(app): Result(WxApp, dynamic.Dynamic) = init_wx()

  // Then run mainloop which is defined by User
  mainloop(app)

  // When all processes are done, run destroy()
  destroy()
}

/// A convenience function that creates a frame, runs a mainloop with it,
/// and handles the setup automatically.
///
/// This function provides a convenient way to work with frames using Gleam's
/// `use` syntax. It creates a frame with the specified title and passes it to
/// your mainloop function. This is useful when you want to encapsulate frame
/// creation and usage in a clean, resource-safe pattern.
///
/// ## Parameters
///
/// - `app` - The WxApp instance obtained from `init_wx()` or `with_app()`
/// - `title` - The title to display in the window's title bar
/// - `mainloop` - A function that receives the WxFrame instance and contains
///   your frame-specific logic. This function will be executed after the frame
///   is created.
///
/// ## Behavior
///
/// 1. Creates a new frame with the specified title (asserts success)
/// 2. Executes your mainloop function with the WxFrame instance
/// 3. Returns after the mainloop completes
///
/// ## Example
///
/// ```gleam
/// use wx_app <- wx_gleam.with_app()
/// use frame <- wx_gleam.with_frame(wx_app, "My Window")
/// 
/// let assert Ok(_button) = create_button(frame, "Click Me!")
/// show_frame(frame)
/// connect_close_event(frame)
/// await_close_message(fn(_) { Nil })
/// ```
///
/// ## Note
///
/// This function will panic if frame creation fails. If you need to handle
/// frame creation errors, use `create_frame()` directly.
pub fn with_frame(
  app: WxApp,
  title: String,
  mainloop: fn(WxFrame) -> Nil,
) -> Nil {
  let assert Ok(frame): Result(WxFrame, Nil) = create_frame(app, title)

  // Run mainloop which is defined by User
  mainloop(frame)
}

/// A convenience function that creates a button, runs a mainloop with it,
/// and handles the setup automatically.
///
/// This function provides a convenient way to work with buttons using Gleam's
/// `use` syntax. It creates a button with the specified label and passes it to
/// your mainloop function. This is useful when you want to encapsulate button
/// creation and usage in a clean, resource-safe pattern.
///
/// ## Parameters
///
/// - `frame` - The WxFrame parent that will contain the button
/// - `label` - The text to display on the button
/// - `mainloop` - A function that receives the WxButton instance and contains
///   your button-specific logic. This function will be executed after the button
///   is created.
///
/// ## Behavior
///
/// 1. Creates a new button with the specified label (asserts success)
/// 2. Executes your mainloop function with the WxButton instance
/// 3. Returns after the mainloop completes
///
/// ## Example
///
/// ```gleam
/// use wx_app <- wx_gleam.with_app()
/// use frame <- wx_gleam.with_frame(wx_app, "My Window")
/// use button <- wx_gleam.with_button(frame, "Click Me!")
/// 
/// // Button is now created and available
/// show_frame(frame)
/// connect_close_event(frame)
/// await_close_message(fn(_) { Nil })
/// ```
///
/// ## Note
///
/// This function will panic if button creation fails. If you need to handle
/// button creation errors, use `create_button()` directly.
pub fn with_button(
  frame: WxFrame,
  label: String,
  mainloop: fn(WxButton) -> Nil,
) -> Nil {
  let assert Ok(button): Result(WxButton, Nil) = create_button(frame, label)

  // Run mainloop which is defined by User
  mainloop(button)
}

/// Creates a new frame (window) with the specified title.
///
/// A frame is a top-level window that can contain other GUI components like
/// buttons, text fields, panels, etc. The frame is created with a default size
/// of 400x300 pixels. The frame is initially hidden and must be shown using
/// `show_frame()`.
///
/// ## Parameters
///
/// - `app` - The WxApp instance obtained from `init_wx()` or `with_app()`
/// - `title` - The title to display in the window's title bar. This will be
///   visible in the operating system's window manager.
///
/// ## Returns
///
/// - `Ok(WxFrame)` - The created frame on success
/// - `Error(Nil)` - An error if frame creation fails (rare, but possible if
///   the wx system is in an invalid state)
///
/// ## Example
///
/// ```gleam
/// let assert Ok(wx_app) = init_wx()
/// let assert Ok(frame) = create_frame(wx_app, "My Application")
/// show_frame(frame)
/// ```
pub fn create_frame(app: WxApp, title: String) -> Result(WxFrame, Nil) {
  internals.create_frame(app, title)
  |> result.map_error(fn(_) { Nil })
}

/// Makes a frame visible on the screen.
///
/// This function displays the frame and makes it visible to the user. The frame
/// will appear on the screen at a position determined by the operating system's
/// window manager. By default, frames are created hidden and must be explicitly
/// shown using this function.
///
/// ## Parameters
///
/// - `frame` - The WxFrame to show. The frame should have been created using
///   `create_frame()` and may contain child widgets like buttons or text fields.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(frame) = create_frame(wx_app, "My Window")
/// show_frame(frame)  // Frame is now visible on screen
/// ```
pub fn show_frame(frame: WxFrame) -> Nil {
  internals.show_frame(frame)
}

/// Creates a button with the specified label inside a frame.
///
/// The button is created as a child widget of the specified frame and will be
/// automatically assigned an ID using wxID_ANY (-1). The button will be visible
/// within the frame's client area.
///
/// ## Parameters
///
/// - `frame` - The parent frame that will contain the button. The button will
///   be positioned within this frame's client area.
/// - `label` - The text to display on the button. This text will be visible to
///   the user and should describe the button's action.
///
/// ## Returns
///
/// - `Ok(WxButton)` - The created button on success
/// - `Error(Nil)` - An error if button creation fails (rare, but possible if
///   the frame is invalid or the wx system is in an invalid state)
///
/// ## Example
///
/// ```gleam
/// let assert Ok(frame) = create_frame(wx_app, "My Window")
/// let assert Ok(button) = create_button(frame, "Click Me!")
/// show_frame(frame)
/// ```
///
/// ## Note
///
/// This is a convenience function that uses `id_any` constant internally. For
/// more control over button IDs or to handle button events, you may need to use
/// the lower-level FFI functions in the `internals` module.
pub fn create_button(frame: WxFrame, label: String) -> Result(WxButton, Nil) {
  internals.create_button(frame, id_any, label)
  |> result.map_error(fn(_) { Nil })
}

/// Connects a close event handler to a frame.
///
/// This sets up the frame to send close_window events when the user attempts
/// to close the window (e.g., by clicking the close button in the title bar or
/// using Alt+F4). After calling this function, you should use 
/// `await_close_message()` to wait for and handle the close event.
///
/// ## Parameters
///
/// - `frame` - The WxFrame to connect the close event to. This should be a
///   frame created with `create_frame()`.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(frame) = create_frame(wx_app, "My Window")
/// show_frame(frame)
/// connect_close_event(frame)
/// await_close_message(fn(_) { Nil })  // Wait for user to close window
/// ```
///
/// ## Note
///
/// Without calling this function, close events will not be sent to your
/// application, and `await_close_message()` will block indefinitely.
pub fn connect_close_event(frame: WxFrame) -> Nil {
  internals.connect_close_event(frame)
}

/// Waits for and handles close messages from the wx application.
///
/// This function blocks the current process until a close_window event is
/// received from the wx system. Any other messages received while waiting
/// will be passed to the provided handler function. This is useful for keeping
/// the application alive until the user closes the window.
///
/// ## Parameters
///
/// - `handler` - A function that will be called with any non-close messages
///   received while waiting. The message is passed as a `dynamic.Dynamic` value
///   which can be decoded if needed. For simple applications, a no-op handler
///   `fn(_) { Nil }` is often sufficient.
///
/// ## Behavior
///
/// 1. Blocks the current process
/// 2. Receives messages from the Erlang message queue
/// 3. If a close_window event is received, the function returns
/// 4. If any other message is received, it's passed to the handler and
///    the function continues waiting
///
/// ## Example
///
/// ```gleam
/// // Simple case: ignore all non-close messages
/// await_close_message(fn(_) { Nil })
/// ```
///
/// ```gleam
/// // Advanced case: handle messages
/// import gleam/io
/// await_close_message(fn(msg) {
///   io.debug(msg)
/// })
/// ```
///
/// ## Note
///
/// This function will block indefinitely if `connect_close_event()` hasn't been
/// called on any frames, as no close messages will be sent.
pub fn await_close_message(handler: fn(dynamic.Dynamic) -> Nil) -> Nil {
  internals.await_close_message(handler)
}

/// Cleans up and destroys the wxWidgets application.
///
/// This function destroys the wx server instance and releases all resources
/// associated with the wx application. It should be called when the application
/// is shutting down to properly clean up resources and prevent memory leaks.
///
/// ## Important Notes
///
/// - This function should be called exactly once at the end of your application
/// - All GUI operations must be completed before calling this function
/// - After calling this function, you cannot create new wx components without
///   calling `init_wx()` again
/// - If using `with_app()`, this is called automatically for you
///
/// ## Example
///
/// ```gleam
/// let assert Ok(wx_app) = init_wx()
/// // ... application code ...
/// destroy()  // Clean up before exiting
/// ```
pub fn destroy() -> Nil {
  internals.destroy()
}
