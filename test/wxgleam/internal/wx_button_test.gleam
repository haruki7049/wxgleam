import gleam/erlang/atom
import gleam/erlang/process
import gleeunit/should
import wxgleam/internal/wx
import wxgleam/internal/wx_button
import wxgleam/internal/wx_frame

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

pub fn new_test() {
  let _new_object: wx.WxObject = wx.new([])
  let new_button: wx.WxObject = wx_button.new(wx.null(), -1, [])

  new_button |> wx.is_null() |> should.be_false()
  new_button |> wx.equal(wx.null()) |> should.be_false()
  new_button
  |> wx.get_object_type()
  |> should.equal(atom.create("wxButton"))

  wx.destroy()
}

pub fn button_show_test() {
  let _new_object: wx.WxObject = wx.new([])
  let button_test_frame: wx.WxObject =
    wx_frame.new(wx.null(), -1, <<"BUTTON TEST FRAME">>, [])
  let default_button: wx.WxObject = wx_button.default()

  default_button |> wx.is_null() |> should.be_false()
  default_button |> wx.equal(wx.null()) |> should.be_false()
  default_button
  |> wx.get_object_type()
  |> should.equal(atom.create("wxButton"))

  let _return: Bool = wx_frame.show(button_test_frame, [])

  process.sleep(500)

  wx.destroy()
}
