import gleam/erlang/process
import gleam/erlang/atom
import gleeunit/should
import wxgleam/internal/wx
import wxgleam/internal/wx_window

pub fn default_test() {
  let _new_object: wx.WxObject = wx.new([])
  let default_window: wx.WxObject = wx_window.default()

  default_window |> wx.is_null() |> should.be_false()
  default_window |> wx.equal(wx.null()) |> should.be_false()
  default_window
  |> wx.get_object_type()
  |> should.equal(atom.create("wxWindow"))

  wx.destroy()
}

pub fn new_test() {
  let _new_object: wx.WxObject = wx.new([])
  let new_window: wx.WxObject = wx_window.new(wx.null(), -1, [])

  new_window |> wx.is_null() |> should.be_false()
  new_window |> wx.equal(wx.null()) |> should.be_false()
  new_window
  |> wx.get_object_type()
  |> should.equal(atom.create("wxWindow"))

  let _return: Bool = wx_window.show(new_window, [])

  process.sleep(500)

  wx.destroy()
}
