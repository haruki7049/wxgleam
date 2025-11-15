//// A Gleam library for creating native GUIs using Erlang's wxWidgets (wx) bindings.
////
//// This module provides a Gleam-friendly API by wrapping the underlying Erlang
//// functions via the Foreign Function Interface (FFI). The core logic is
//// implemented in Erlang and exposed to Gleam through FFI definitions.
////
//// ## Example
////
//// ```gleam
//// import wx_gleam
////
//// pub fn main() {
////   use wx_app <- wx_gleam.with_app()
////   let assert Ok(wx_frame) = wx_gleam.create_frame(wx_app, "My App")
////   wx_gleam.show_frame(wx_frame)
////   wx_gleam.connect_close_event(wx_frame)
////   wx_gleam.apply_message_handler(fn(_) { Nil })
//// }
//// ```

import gleam/dynamic
import gleam/result
import wx_gleam/internals

// Re-export wx types and decoders for convenience
pub type WxMessage {
  WxMessage(
    id: Int,
    obj: dynamic.Dynamic,
    user_data: dynamic.Dynamic,
    event: WxEvent,
  )
}

pub type WxEvent {
  WxClose(event_type: String)
  WxCommand(event_type: String, command_int: Int, command_string: String)
  WxFocus(event_type: String)
  WxKey(event_type: String, key_code: Int)
  WxMouse(event_type: String, x: Int, y: Int)
  WxSize(event_type: String, width: Int, height: Int)
  WxUnknown(data: dynamic.Dynamic)
}

// --- Type definitions ---

/// Represents a wxWidgets application instance.
///
/// This is the main entry point for any wxWidgets application and must be
/// initialized before creating any GUI components.
pub type WxApp =
  internals.WxApp

/// Represents a wxWidgets frame (window).
///
/// A frame is a top-level window that can contain other GUI components like
/// buttons, text fields, etc.
pub type WxFrame =
  internals.WxFrame

/// Represents a wxWidgets button control.
///
/// A button is a GUI control that can be clicked by the user to trigger actions.
pub type WxButton =
  internals.WxButton

// --- Constants ---

/// A constant representing wxID_ANY (-1).
///
/// This is used to let wxWidgets automatically assign an ID to a widget.
pub const id_any = -1

/// Initializes a wxWidgets application.
///
/// This function must be called before creating any GUI components.
/// It returns a Result containing the WxApp instance on success, or a
/// dynamic error on failure.
///
/// ## Returns
///
/// - `Ok(WxApp)` - The initialized application instance
/// - `Error(dynamic.Dynamic)` - An error if initialization fails
pub fn init_wx() -> Result(WxApp, dynamic.Dynamic) {
  internals.init_wx()
}

/// A convenience function that initializes the wx application, runs a mainloop,
/// and cleans up automatically.
///
/// This function handles the boilerplate of initializing wx, running your
/// application logic, and cleaning up when done.
///
/// ## Parameters
///
/// - `mainloop` - A function that receives the WxApp instance and contains
///   your application logic
///
/// ## Example
///
/// ```gleam
/// use wx_app <- wx_gleam.with_app()
/// // Your application code here
/// ```
pub fn with_app(mainloop: fn(WxApp) -> Nil) -> Nil {
  let assert Ok(app): Result(WxApp, dynamic.Dynamic) = init_wx()

  // Then run mainloop which is defined by User
  mainloop(app)

  // When all processes are done, run destroy()
  destroy()
}

/// Creates a new frame (window) with the specified title.
///
/// ## Parameters
///
/// - `app` - The WxApp instance
/// - `title` - The title to display in the window's title bar
///
/// ## Returns
///
/// - `Ok(WxFrame)` - The created frame on success
/// - `Error(Nil)` - An error if frame creation fails
pub fn create_frame(app: WxApp, title: String) -> Result(WxFrame, Nil) {
  internals.create_frame(app, title)
  |> result.map_error(fn(_) { Nil })
}

/// Makes a frame visible on the screen.
///
/// ## Parameters
///
/// - `frame` - The WxFrame to show
pub fn show_frame(frame: WxFrame) -> Nil {
  internals.show_frame(frame)
}

/// Creates a button with the specified label inside a frame.
///
/// The button will be automatically assigned an ID using wxID_ANY.
///
/// ## Parameters
///
/// - `frame` - The parent frame that will contain the button
/// - `label` - The text to display on the button
///
/// ## Returns
///
/// - `Ok(WxButton)` - The created button on success
/// - `Error(Nil)` - An error if button creation fails
pub fn create_button(frame: WxFrame, label: String) -> Result(WxButton, Nil) {
  internals.create_button(frame, id_any, label)
  |> result.map_error(fn(_) { Nil })
}

/// Connects a close event handler to a frame.
///
/// This sets up the frame to receive close events when the user attempts
/// to close the window.
///
/// ## Parameters
///
/// - `frame` - The WxFrame to connect the close event to
pub fn connect_close_event(frame: WxFrame) -> Nil {
  internals.connect_close_event(frame)
}

/// Handles wx's messages from the wx application.
///
/// This function blocks until a close message is received, then calls the
/// provided handler function with the message.
///
/// ## Parameters
///
/// - `handler` - A function that will be called with the close message
pub fn apply_message_handler(handler: fn(dynamic.Dynamic) -> Nil) -> Nil {
  internals.apply_message_handler(handler)
}

/// Cleans up and destroys the wxWidgets application.
///
/// This should be called when the application is shutting down to properly
/// clean up resources.
pub fn destroy() -> Nil {
  internals.destroy()
}
