import gleam/erlang/atom
import gleam/dynamic

pub type WxObject

@external(erlang, "application", "ensure_all_started")
fn ensure_all_started(app: atom.Atom) -> Result(List(atom.Atom), dynamic.Dynamic)

pub type NewOption

@external(erlang, "wx", "new")
pub fn new(options: List(NewOption)) -> WxObject

@external(erlang, "wx", "null")
pub fn null() -> WxObject

@external(erlang, "wx", "is_null")
pub fn is_null(object: WxObject) -> Bool
