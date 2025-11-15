//// Example application demonstrating the wx_gleam library.
////
//// This example creates a simple wxWidgets window with a button and demonstrates
//// the basic usage of the wx_gleam API including:
//// - Initializing a wx application
//// - Creating a frame (window)
//// - Adding a button to the frame
//// - Handling close events
//// - Cleaning up resources

import gleam/dynamic
import gleam/dynamic/decode
import gleam/io
import gleam/list
import gleam/string
import wx_gleam
import wx_gleam/wx_decode

/// Entry point for the example application.
///
/// Creates a simple wxWidgets window with a "Click Me!" button and waits
/// for the user to close the window.
pub fn main() {
  use wx_app: wx_gleam.WxApp <- wx_gleam.with_app()

  let assert Ok(wx_frame) = wx_gleam.create_frame(wx_app, "Gleam WxApp")
  let assert Ok(_button) = wx_gleam.create_button(wx_frame, "Click Me!")

  wx_gleam.show_frame(wx_frame)

  wx_gleam.connect_close_event(wx_frame)
  wx_gleam.apply_message_handler(message_handler)
}

/// Handles messages from the wx application.
///
/// This function is called when the window receives any wx event. It attempts
/// to decode the message as a WxMessage and prints information about the event.
/// If decoding fails, it prints the error to stderr.
///
/// ## Parameters
///
/// - `message` - The dynamic message received from wx events
fn message_handler(message: dynamic.Dynamic) -> Nil {
  let result = decode.run(message, wx_decode.wx_message())

  case result {
    Ok(wx_msg) -> {
      io.println("Received wx message:")
      io.println("  ID: " <> string.inspect(wx_msg.id))

      case wx_msg.event {
        wx_gleam.WxClose(event_type) -> {
          io.println("  Event: Close (" <> event_type <> ")")
        }
        wx_gleam.WxCommand(event_type, cmd_int, cmd_str) -> {
          io.println("  Event: Command (" <> event_type <> ")")
          io.println("  Command Int: " <> string.inspect(cmd_int))
          io.println("  Command String: " <> cmd_str)
        }
        wx_gleam.WxFocus(event_type) -> {
          io.println("  Event: Focus (" <> event_type <> ")")
        }
        wx_gleam.WxKey(event_type, key_code) -> {
          io.println("  Event: Key (" <> event_type <> ")")
          io.println("  Key Code: " <> string.inspect(key_code))
        }
        wx_gleam.WxMouse(event_type, x, y) -> {
          io.println("  Event: Mouse (" <> event_type <> ")")
          io.println(
            "  Position: ("
            <> string.inspect(x)
            <> ", "
            <> string.inspect(y)
            <> ")",
          )
        }
        wx_gleam.WxSize(event_type, width, height) -> {
          io.println("  Event: Size (" <> event_type <> ")")
          io.println(
            "  Size: " <> string.inspect(width) <> "x" <> string.inspect(height),
          )
        }
        wx_gleam.WxUnknown(_) -> {
          io.println("  Event: Unknown/Unsupported")
        }
      }
    }
    Error(err_list) -> {
      io.println_error("Failed to decode wx message:")
      err_list
      |> decode_errors_to_string()
      |> list.each(io.println_error)
    }
  }
}

fn decode_errors_to_string(errors: List(decode.DecodeError)) -> List(String) {
  errors
  |> list.map(fn(v: decode.DecodeError) -> String {
    let paths: String =
      v.path
      |> string.join(", ")

    "Expected: " <> v.expected <> ", Found: " <> v.found <> ", Paths: " <> paths
  })
}
