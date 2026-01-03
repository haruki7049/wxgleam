import gleam/erlang/atom
import gleeunit/should
import wxgleam/internal/wx
import wxgleam/internal/wx_button

pub fn default_test() {
  let _new_object: wx.WxObject = wx.new([])
  let default_button: wx.WxObject = wx_button.default()

  default_button |> wx.is_null() |> should.be_false()
  default_button |> wx.equal(wx.null()) |> should.be_false()
  default_button
  |> wx.get_object_type()
  |> should.equal(atom.create("wxButton"))

  wx.destroy()
}
