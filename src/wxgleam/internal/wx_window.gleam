import wxgleam/internal/wx

@external(erlang, "wxWindow", "new")
pub fn default() -> wx.WxObject

pub type NewOption

@external(erlang, "wxWindow", "new")
pub fn new(
  parent: wx.WxObject,
  id: Int,
  options: List(NewOption),
) -> wx.WxObject

pub type ShowOption

@external(erlang, "wxWindow", "show")
pub fn show(this: wx.WxObject, options: List(ShowOption)) -> Bool
