import gleam/dynamic

pub type WxApp

pub type WxFrame

pub type WxButton

@external(erlang, "wx_ffi", "init_wx")
pub fn init_wx() -> Result(WxApp, dynamic.Dynamic)

@external(erlang, "wx_ffi", "create_frame")
pub fn create_frame(
  app: WxApp,
  title: String,
) -> Result(WxFrame, dynamic.Dynamic)

@external(erlang, "wx_ffi", "show_frame")
pub fn show_frame(frame: WxFrame) -> Nil

@external(erlang, "wx_ffi", "create_button")
pub fn create_button(
  frame: WxFrame,
  id: Int,
  label: String,
) -> Result(WxButton, dynamic.Dynamic)

@external(erlang, "wx_ffi", "connect_close_event")
pub fn connect_close_event(frame: WxFrame) -> Nil

@external(erlang, "wx_ffi", "await_close_message")
pub fn await_close_message(handler: fn(dynamic.Dynamic) -> Nil) -> Nil

@external(erlang, "wx_ffi", "destroy")
pub fn destroy() -> Nil
