//// Decoders for wxWidgets messages and events.
////
//// This module provides dynamic decoders for wx message types, allowing
//// proper parsing of Erlang wx records into Gleam types.
////
//// The decoders handle the conversion from Erlang's record format to
//// Gleam's type-safe structures.
////
//// ## Example
////
//// ```gleam
//// import wx_gleam/wx_decode
//// import gleam/dynamic/decode
////
//// fn handle_message(msg: dynamic.Dynamic) -> Nil {
////   case decode.run(msg, wx_decode.wx_message()) {
////     Ok(wx_msg) -> {
////       case wx_msg.event {
////         wx_gleam.WxClose(event_type) -> {
////           io.println("Window is closing: " <> event_type)
////         }
////         wx_gleam.WxCommand(event_type, _, _) -> {
////           io.println("Command received: " <> event_type)
////         }
////         _ -> Nil
////       }
////     }
////     Error(_) -> io.println_error("Failed to decode wx message")
////   }
//// }
//// ```

import gleam/dynamic
import gleam/dynamic/decode
import gleam/string
import wx_gleam.{type WxEvent, type WxMessage}

/// Decodes a wx message from a dynamic value.
///
/// This decoder attempts to parse an Erlang #wx{} record into a WxMessage type.
/// It extracts the id, obj, user_data, and event fields from the record.
///
/// ## Example
///
/// ```gleam
/// case decode.run(message, wx_message()) {
///   Ok(wx_msg) -> // Handle the parsed message
///   Error(_) -> // Handle decode error
/// }
/// ```
pub fn wx_message() -> decode.Decoder(WxMessage) {
  decode.decode4(
    wx_gleam.WxMessage,
    decode.element(1, decode.int),
    decode.element(2, decode.dynamic),
    decode.element(3, decode.dynamic),
    decode.element(4, wx_event()),
  )
}

/// Decodes a wx event from a dynamic value.
///
/// This decoder attempts to parse various wx event records into the WxEvent type.
/// It tries to match the event record structure and extract relevant fields.
pub fn wx_event() -> decode.Decoder(WxEvent) {
  fn(dyn: dynamic.Dynamic) -> Result(WxEvent, List(decode.DecodeError)) {
    // Try to decode as wxClose event
    case decode.run(dyn, wx_close_event()) {
      Ok(close_event) -> Ok(close_event)
      Error(_) ->
        // Try to decode as wxCommand event
        case decode.run(dyn, wx_command_event()) {
          Ok(command_event) -> Ok(command_event)
          Error(_) ->
            // Try other event types, or return WxUnknown
            Ok(wx_gleam.WxUnknown(dyn))
        }
    }
  }
}

/// Decodes a wxClose event.
fn wx_close_event() -> decode.Decoder(WxEvent) {
  // #wxClose{type = Type} => {wxClose, Type}
  decode.decode1(
    fn(event_type) { wx_gleam.WxClose(string.inspect(event_type)) },
    decode.element(1, decode.dynamic),
  )
}

/// Decodes a wxCommand event.
fn wx_command_event() -> decode.Decoder(WxEvent) {
  // #wxCommand{type = Type, commandInt = Int, commandString = String}
  decode.decode3(
    fn(event_type, cmd_int, cmd_str) {
      wx_gleam.WxCommand(string.inspect(event_type), cmd_int, cmd_str)
    },
    decode.element(1, decode.dynamic),
    decode.element(2, decode.int),
    decode.element(3, decode.string),
  )
}

/// Simple decoder that just captures the event as dynamic data.
///
/// This is useful when you want to handle the raw Erlang message
/// without parsing it into specific event types.
pub fn dynamic_message() -> decode.Decoder(dynamic.Dynamic) {
  decode.dynamic
}
