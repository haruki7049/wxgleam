import wx_gleam

pub fn main() {
  let assert Ok(wx_app) = wx_gleam.init_wx()
  let assert Ok(wx_frame) = wx_gleam.create_frame(wx_app, "Gleam WxApp")
  let assert Ok(_button) = wx_gleam.create_button(wx_frame, "Click Me!")

  wx_gleam.show_frame(wx_frame)

  wx_gleam.connect_close_event(wx_frame)
  wx_gleam.await_close_message()

  wx_gleam.destroy()
}
