//// Internal FFI bindings for wxWidgets functionality.
////
//// This module contains the low-level Foreign Function Interface (FFI) bindings
//// to the Erlang wx module. These functions provide direct access to the Erlang
//// implementation in `wx_ffi.erl` and are wrapped by the more user-friendly
//// public API in the main wx_gleam module.
////
//// ## For Library Users
////
//// **Note:** Users should generally use the functions in the wx_gleam module
//// instead of calling these internal functions directly. The public API provides
//// better error handling, more convenient function signatures, and higher-level
//// abstractions.
////
//// ## For Library Developers
////
//// This module defines:
//// - Opaque types for wx objects (WxApp, WxFrame, WxButton, WxCheckBox)
//// - FFI function declarations using the `@external` attribute
//// - Direct mappings to Erlang wx_ffi module functions
////
//// All functions in this module return `Result` types with `dynamic.Dynamic`
//// errors or `Nil` for operations that don't fail. The public API in wx_gleam
//// wraps these to provide more user-friendly error types.

import gleam/dynamic

/// Opaque type representing a wxWidgets application instance.
///
/// This type is implemented in Erlang and represents the underlying wx
/// application object (the wx server process). It is an opaque reference
/// that cannot be constructed directly in Gleam - it must be obtained through
/// the `init_wx()` FFI function.
pub type WxApp

/// Opaque type representing a wxWidgets frame (window).
///
/// This type is implemented in Erlang and represents the underlying wx
/// frame object. It is an opaque reference to a wxFrame widget in the
/// Erlang wx system.
pub type WxFrame

/// Opaque type representing a wxWidgets button control.
///
/// This type is implemented in Erlang and represents the underlying wx
/// button object. It is an opaque reference to a wxButton widget in the
/// Erlang wx system.
pub type WxButton

/// Opaque type representing a wxWidgets text control.
///
/// This type is implemented in Erlang and represents the underlying wx
/// text control object. It is an opaque reference to a wxTextCtrl widget
/// in the Erlang wx system.
pub type WxTextCtrl

/// Opaque type representing a wxWidgets checkbox control.
///
/// This type is implemented in Erlang and represents the underlying wx
/// checkbox object. It is an opaque reference to a wxCheckBox widget
/// in the Erlang wx system.
pub type WxCheckBox

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

/// Waits for and handles close messages from the wx application via FFI.
///
/// This is the Erlang FFI binding that blocks until a close message is received.
///
/// ## Parameters
///
/// - `handler` - A function that will be called with the close message
@external(erlang, "wx_ffi", "await_close_message")
pub fn await_close_message(handler: fn(dynamic.Dynamic) -> Nil) -> Nil

/// Creates a text control with the specified ID and initial value inside a frame via FFI.
///
/// This is the Erlang FFI binding that creates a wx text control.
///
/// ## Parameters
///
/// - `frame` - The parent frame that will contain the text control
/// - `id` - The widget ID (use -1 for wxID_ANY)
/// - `value` - The initial text to display in the text control
///
/// ## Returns
///
/// - `Ok(WxTextCtrl)` - The created text control on success
/// - `Error(dynamic.Dynamic)` - An error if text control creation fails
@external(erlang, "wx_ffi", "create_text_ctrl")
pub fn create_text_ctrl(
  frame: WxFrame,
  id: Int,
  value: String,
) -> Result(WxTextCtrl, dynamic.Dynamic)

/// Creates a checkbox with the specified ID and label inside a frame via FFI.
///
/// This is the Erlang FFI binding that creates a wx checkbox.
///
/// ## Parameters
///
/// - `frame` - The parent frame that will contain the checkbox
/// - `id` - The widget ID (use -1 for wxID_ANY)
/// - `label` - The text to display next to the checkbox
///
/// ## Returns
///
/// - `Ok(WxCheckBox)` - The created checkbox on success
/// - `Error(dynamic.Dynamic)` - An error if checkbox creation fails
@external(erlang, "wx_ffi", "create_checkbox")
pub fn create_checkbox(
  frame: WxFrame,
  id: Int,
  label: String,
) -> Result(WxCheckBox, dynamic.Dynamic)

/// Cleans up and destroys the wxWidgets application via FFI.
///
/// This is the Erlang FFI binding that destroys the wx application.
@external(erlang, "wx_ffi", "destroy")
pub fn destroy() -> Nil
