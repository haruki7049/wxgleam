import wxgleam/internal/wx

@external(erlang, "wxButton", "new")
pub fn default() -> wx.WxObject

pub type NewOption

@external(erlang, "wxButton", "new")
pub fn new(
  parent: wx.WxObject,
  id: Int,
  options: List(NewOption),
) -> wx.WxObject

pub type CreateOption

@external(erlang, "wxButton", "create")
pub fn create(
  this: wx.WxObject,
  parent: wx.WxObject,
  id: Int,
  options: List(CreateOption),
) -> Bool
