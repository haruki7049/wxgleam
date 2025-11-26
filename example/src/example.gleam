//// Example application demonstrating the wx_gleam library.
////
//// This example creates a simple wxWidgets window with a button and demonstrates
//// the basic usage of the wx_gleam API. It showcases the recommended pattern
//// using the `with_app` and `with_frame` convenience functions for automatic 
//// resource management.
////
//// ## What This Example Demonstrates
////
//// - Using `with_app()` for automatic initialization and cleanup
//// - Using `with_frame()` for automatic frame creation with `use` syntax
//// - Adding a button widget to the frame
//// - Making the window visible with `show_frame()`
//// - Connecting close event handlers
//// - Handling close messages with a custom handler function
////
//// ## Running the Example
////
//// From the example directory:
//// ```sh
//// gleam run
//// ```
////
//// The application will display a window with a "Click Me!" button. Close the
//// window to exit the application.

import gleam/io
import wx_gleam
import wx_gleam/events

/// Entry point for the example application.
///
/// Creates a simple wxWidgets window with a "Click Me!" button and waits
/// for the user to close the window. This function demonstrates the recommended
/// pattern using `with_app()`, `with_frame()`, and `with_button()` which 
/// automatically handle wx initialization, frame creation, and button creation.
///
/// ## Application Flow
///
/// 1. Initialize wx application using `with_app()`
/// 2. Create a 400x300 pixel frame titled "Gleam WxApp" using `with_frame()`
/// 3. Add a button labeled "Click Me!" to the frame using `with_button()`
/// 4. Make the frame visible on screen
/// 5. Connect the close event handler
/// 6. Wait for user to close the window (blocking call)
/// 7. Automatic cleanup by `with_app()`
pub fn main() {
  use wx_app: wx_gleam.WxApp <- wx_gleam.with_app()
  use frame <- wx_gleam.with_frame(wx_app, "Gleam WxApp")
  use _button <- wx_gleam.with_button(frame, "Click Me!")

  frame
  |> wx_gleam.show_frame()
  |> wx_gleam.connect_close_event()

  wx_gleam.await_close_event(event_handler)
}

/// Handles typed close events from the wx application.
///
/// This function is called when the window receives a close event. It receives
/// a typed `CloseEvent` which can be either a successfully decoded close message
/// or an unknown event if decoding failed.
///
/// ## Parameters
///
/// - `event` - The typed CloseEvent received from the close event handler.
///
/// ## Behavior
///
/// - **Close event**: Prints the message to stdout
/// - **Unknown event**: Prints the raw value to stderr (this indicates a decoding failure)
///
/// ## Note
///
/// For most applications, a simpler handler like `fn(_) { Nil }` is sufficient.
/// This example demonstrates event handling for educational purposes.
fn event_handler(event: events.CloseEvent) -> Nil {
  case event {
    events.Close(message) -> io.println("Close event received: " <> message)
    events.Unknown(raw) -> io.println_error("Unknown event: " <> raw)
  }
}
