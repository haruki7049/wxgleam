import gleam/erlang/atom

pub type WxObject

pub type NewOption {
  SilentStart(Bool)
}

@external(erlang, "wx", "new")
pub fn new(options: List(NewOption)) -> WxObject

@external(erlang, "wx", "null")
pub fn null() -> WxObject

@external(erlang, "wx", "is_null")
pub fn is_null(object: WxObject) -> Bool

@external(erlang, "wx", "equal")
pub fn equal(left: WxObject, right: WxObject) -> Bool

@external(erlang, "wx", "getObjectType")
pub fn get_object_type(object: WxObject) -> atom.Atom

@external(erlang, "wx", "destroy")
pub fn destroy() -> Nil
