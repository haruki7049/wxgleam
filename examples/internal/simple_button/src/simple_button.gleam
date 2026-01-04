import gleam/erlang/process
import wxgleam/internal/wx
import wxgleam/internal/wx_button
import wxgleam/internal/wx_frame

pub fn main() -> Nil {
  let _new_object: wx.WxObject = wx.new([])
  let button_test_frame: wx.WxObject =
    wx_frame.new(wx.null(), -1, <<"BUTTON TEST FRAME">>, [])
  let _button_creation: Bool =
    wx_button.create(wx.null(), button_test_frame, -1, [])

  let _return: Bool = wx_frame.show(button_test_frame, [])

  process.sleep(1000)

  wx.destroy()
}
