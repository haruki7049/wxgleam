import gleam/erlang/process
import wxgleam/internal/wx
import wxgleam/internal/wx_frame

pub fn main() -> Nil {
  let _new_object: wx.WxObject = wx.new([])
  let new_frame: wx.WxObject = wx_frame.new(wx.null(), -1, <<"HOGE">>, [])

  let _return: Bool = wx_frame.show(new_frame, [])

  process.sleep(500)

  wx.destroy()
}
