//// Internal FFI bindings for wxWidgets functionality.
////
//// This module contains the low-level Foreign Function Interface (FFI) bindings
//// to the Erlang wx module. These functions are wrapped by the public API in
//// the main wx_gleam module.
////
//// **Note:** Users should generally use the functions in the wx_gleam module
//// instead of calling these internal functions directly.

import gleam/dynamic

/// Opaque type representing a wxWidgets application instance.
///
/// This type is implemented in Erlang and represents the underlying wx
/// application object.
pub type WxApp

/// Opaque type representing a wxWidgets frame (window).
///
/// This type is implemented in Erlang and represents the underlying wx
/// frame object.
pub type WxFrame

/// Opaque type representing a wxWidgets button control.
///
/// This type is implemented in Erlang and represents the underlying wx
/// button object.
pub type WxButton

/// Initializes the wxWidgets application via FFI.
///
/// This is the Erlang FFI binding that creates and initializes a wx application.
///
/// ## Returns
///
/// - `Ok(WxApp)` - The initialized application instance
/// - `Error(dynamic.Dynamic)` - An error if initialization fails
@external(erlang, "wx_ffi", "init_wx")
pub fn init_wx() -> Result(WxApp, dynamic.Dynamic)

/// Creates a new frame (window) with the specified title via FFI.
///
/// This is the Erlang FFI binding that creates a wx frame.
///
/// ## Parameters
///
/// - `app` - The WxApp instance
/// - `title` - The title to display in the window's title bar
///
/// ## Returns
///
/// - `Ok(WxFrame)` - The created frame on success
/// - `Error(dynamic.Dynamic)` - An error if frame creation fails
@external(erlang, "wx_ffi", "create_frame")
pub fn create_frame(
  app: WxApp,
  title: String,
) -> Result(WxFrame, dynamic.Dynamic)

/// Makes a frame visible on the screen via FFI.
///
/// This is the Erlang FFI binding that shows a wx frame.
///
/// ## Parameters
///
/// - `frame` - The WxFrame to show
@external(erlang, "wx_ffi", "show_frame")
pub fn show_frame(frame: WxFrame) -> Nil

/// Creates a button with the specified ID and label inside a frame via FFI.
///
/// This is the Erlang FFI binding that creates a wx button.
///
/// ## Parameters
///
/// - `frame` - The parent frame that will contain the button
/// - `id` - The widget ID (use -1 for wxID_ANY)
/// - `label` - The text to display on the button
///
/// ## Returns
///
/// - `Ok(WxButton)` - The created button on success
/// - `Error(dynamic.Dynamic)` - An error if button creation fails
@external(erlang, "wx_ffi", "create_button")
pub fn create_button(
  frame: WxFrame,
  id: Int,
  label: String,
) -> Result(WxButton, dynamic.Dynamic)

/// Connects a close event handler to a frame via FFI.
///
/// This is the Erlang FFI binding that sets up close event handling.
///
/// ## Parameters
///
/// - `frame` - The WxFrame to connect the close event to
@external(erlang, "wx_ffi", "connect_close_event")
pub fn connect_close_event(frame: WxFrame) -> Nil

/// Handles wx's messages from the wx application via FFI.
///
/// This is the Erlang FFI binding that blocks until a close message is received.
///
/// ## Parameters
///
/// - `handler` - A function that will be called with the close message
@external(erlang, "wx_ffi", "apply_message_handler")
pub fn apply_message_handler(handler: fn(dynamic.Dynamic) -> Nil) -> Nil

/// Cleans up and destroys the wxWidgets application via FFI.
///
/// This is the Erlang FFI binding that destroys the wx application.
@external(erlang, "wx_ffi", "destroy")
pub fn destroy() -> Nil
