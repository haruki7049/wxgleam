//// Event types and decoders for wx_gleam.
////
//// This module provides type-safe event handling for wxWidgets events by
//// defining event types and decoders that convert dynamic Erlang messages
//// into strongly-typed Gleam values.
////
//// ## Event Types
////
//// Currently supports:
//// - `CloseEvent` - Events related to window close actions
////
//// ## Usage
////
//// The event types and decoders in this module are primarily used internally
//// by the main wx_gleam module to provide type-safe event handlers to users.
//// However, you can also use them directly if you need custom event handling.

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/string

// Constants for close event type strings
/// Constant for the close_window event type string
pub const close_window_event = "close_window"

/// Constant for the end_session event type string
pub const end_session_event = "end_session"

/// Constant for the query_end_session event type string
pub const query_end_session_event = "query_end_session"

/// Represents the type of close event.
///
/// Different close event types indicate different reasons why a window or
/// application is being closed.
///
/// ## Variants
///
/// - `CloseWindow` - User initiated close (e.g., clicked close button, pressed Alt+F4)
/// - `EndSession` - System is shutting down or logging out
/// - `QueryEndSession` - System is querying if it's safe to end the session
pub type CloseEventType {
  /// User initiated close action.
  ///
  /// This event is sent when the user clicks the close button, presses Alt+F4,
  /// or otherwise requests the window to close.
  CloseWindow
  /// System is ending the session.
  ///
  /// This event is sent when the system is shutting down or the user is logging out.
  /// The application should save its state and close gracefully.
  EndSession
  /// System is querying about ending the session.
  ///
  /// This event is sent when the system wants to know if it's safe to end the session.
  /// The application can indicate whether it can close without data loss.
  QueryEndSession
}

/// Represents a close window event.
///
/// This type captures events that occur when a user attempts to close a window,
/// such as clicking the close button or pressing Alt+F4, or when the system
/// is shutting down.
///
/// ## Variants
///
/// - `Close(type)` - A successfully decoded close event with the event type
/// - `Unknown(raw)` - A fallback for when decoding fails, containing the raw string
///   representation of the message for debugging purposes
pub type CloseEvent {
  /// A successfully decoded close event.
  ///
  /// The `type` field indicates what kind of close event occurred.
  Close(event_type: CloseEventType)
  /// A fallback event used when decoding fails.
  ///
  /// The `raw` field contains a string representation of the dynamic value that
  /// could not be decoded, useful for debugging and logging purposes.
  Unknown(raw: String)
}

/// Decodes a dynamic message into a CloseEvent.
///
/// This function attempts to decode an incoming dynamic Erlang message into a
/// typed `CloseEvent`. It expects messages in the format `{close, Type}` where
/// Type is an atom indicating the close event subtype (close_window, end_session,
/// or query_end_session).
///
/// ## Parameters
///
/// - `msg` - The dynamic message received from the wx event system, typically
///   from the Erlang message queue. Should be a tuple `{close, type_atom}`.
///
/// ## Returns
///
/// - `Ok(Close(event_type))` - When the message is successfully decoded
/// - `Error(error_description)` - When decoding fails, with a description of the
///   error including the inspected raw value
///
/// ## Example
///
/// ```gleam
/// case decode_close_event(dynamic_msg) {
///   Ok(event) -> handle_event(event)
///   Error(err) -> io.println_error("Failed to decode: " <> err)
/// }
/// ```
///
/// ## Note
///
/// Most users will not call this function directly. Instead, use the
/// `await_close_event` function in the main wx_gleam module, which handles
/// decoding automatically and provides a typed handler interface.
pub fn decode_close_event(msg: Dynamic) -> Result(CloseEvent, String) {
  // Decoder for 2-element tuples where both elements are strings (atoms decode to strings)
  // Using decode.element to access tuple elements at positions 0 and 1
  let tuple_decoder = {
    use tag <- decode.element(0, decode.string)
    use type_str <- decode.element(1, decode.string)
    decode.success(#(tag, type_str))
  }

  case decode.run(msg, tuple_decoder) {
    Ok(#("close", type_str)) -> {
      // Decode the type string to CloseEventType using constants
      case type_str {
        _ if type_str == close_window_event -> Ok(Close(CloseWindow))
        _ if type_str == end_session_event -> Ok(Close(EndSession))
        _ if type_str == query_end_session_event -> Ok(Close(QueryEndSession))
        _ -> {
          let raw = string.inspect(msg)
          Error(
            "Failed to decode close event. Unknown type: "
            <> type_str
            <> ". Raw value: "
            <> raw,
          )
        }
      }
    }
    Ok(_) -> {
      // Handle tuples that don't match #("close", _)
      let raw = string.inspect(msg)
      Error(
        "Failed to decode close event. Expected {close, Type} tuple. Raw value: "
        <> raw,
      )
    }
    Error(_decode_errors) -> {
      // When decoding fails, provide a helpful error message with the raw value
      let raw = string.inspect(msg)
      Error(
        "Failed to decode close event. Expected {close, Type} tuple. Raw value: "
        <> raw,
      )
    }
  }
}
