import wxgleam/internal/wx

@external(erlang, "wxFrame", "new")
pub fn default() -> wx.WxObject

pub type NewOption {
  Pos(#(Int, Int))
  Size(#(Int, Int))
  Style(Int)
}

@external(erlang, "wxFrame", "new")
pub fn new(
  parent: wx.WxObject,
  id: Int,
  title: BitArray,
  options: List(NewOption),
) -> wx.WxObject

pub type ShowOption {
  Show(Bool)
}

@external(erlang, "wxFrame", "show")
pub fn show(this: wx.WxObject, options: List(ShowOption)) -> Bool
