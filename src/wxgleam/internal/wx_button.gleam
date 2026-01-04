import wxgleam/internal/wx

@external(erlang, "wxButton", "new")
pub fn default() -> wx.WxObject

pub type CreateOption {
  Label(BitArray)
  Pos(#(Int, Int))
  Size(#(Int, Int))
  Style(Int)
  Validator(wx.WxObject)
}

@external(erlang, "wxButton", "new")
pub fn new(
  parent: wx.WxObject,
  id: Int,
  options: List(CreateOption),
) -> wx.WxObject

@external(erlang, "wxButton", "create")
pub fn create(
  this: wx.WxObject,
  parent: wx.WxObject,
  id: Int,
  options: List(CreateOption),
) -> Bool
