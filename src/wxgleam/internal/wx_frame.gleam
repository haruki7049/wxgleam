import wxgleam/internal/wx

@external(erlang, "wxFrame", "new")
pub fn default() -> wx.WxObject

pub type NewOption

@external(erlang, "wxFrame", "new")
pub fn new(parent: wx.WxObject, id: Int, title: BitArray, options: List(NewOption)) -> wx.WxObject

pub type ShowOption

@external(erlang, "wxFrame", "show")
pub fn show(this: wx.WxObject, options: List(ShowOption)) -> Bool
