import gleam/erlang/atom
import gleam/erlang/process
import gleeunit/should
import wxgleam/internal/wx
import wxgleam/internal/wx_frame

pub fn default_test() {
  let _new_object: wx.WxObject = wx.new([])
  let default_frame: wx.WxObject = wx_frame.default()

  default_frame |> wx.is_null() |> should.be_false()
  default_frame |> wx.equal(wx.null()) |> should.be_false()
  default_frame
  |> wx.get_object_type()
  |> should.equal(atom.create("wxFrame"))

  wx.destroy()
}

pub fn new_test() {
  let _new_object: wx.WxObject = wx.new([])
  let new_frame: wx.WxObject = wx_frame.new(wx.null(), -1, <<"HOGE">>, [])

  new_frame |> wx.is_null() |> should.be_false()
  new_frame |> wx.equal(wx.null()) |> should.be_false()
  new_frame
  |> wx.get_object_type()
  |> should.equal(atom.create("wxFrame"))

  let _return: Bool = wx_frame.show(new_frame, [])

  process.sleep(500)

  wx.destroy()
}
