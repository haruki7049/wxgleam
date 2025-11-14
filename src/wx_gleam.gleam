import gleam/dynamic
import gleam/result
import wx_gleam/internals

// --- Type definitions ---
pub type WxApp =
  internals.WxApp

pub type WxFrame =
  internals.WxFrame

pub type WxButton =
  internals.WxButton

// --- Constants ---
// Use -1 for wxID_ANY
pub const id_any = -1

pub fn init_wx() -> Result(WxApp, dynamic.Dynamic) {
  internals.init_wx()
}

pub fn create_frame(app: WxApp, title: String) -> Result(WxFrame, Nil) {
  internals.create_frame(app, title)
  |> result.map_error(fn(_) { Nil })
}

pub fn show_frame(frame: WxFrame) -> Nil {
  internals.show_frame(frame)
}

pub fn create_button(frame: WxFrame, label: String) -> Result(WxButton, Nil) {
  internals.create_button(frame, id_any, label)
  |> result.map_error(fn(_) { Nil })
}

pub fn connect_close_event(frame: WxFrame) -> Nil {
  internals.connect_close_event(frame)
}

pub fn await_close_message() -> Nil {
  internals.await_close_message()
}

pub fn destroy() -> Nil {
  internals.destroy()
}
