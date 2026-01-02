import gleam/erlang/atom
import gleam/dynamic

pub type WxObject {
  WxEvtHandler(inner: WxEvtHandler)
}

pub type WxEvtHandler {
  WxWindow()
  WxAppConsole(inner: WxAppConsole)
}

pub type WxAppConsole {
  WxApp()
}

@external(erlang, "application", "ensure_all_started")
fn ensure_all_started(app: atom.Atom) -> Result(List(atom.Atom), dynamic.Dynamic)

pub type NewOption {
  Debug(Level)
  SilentStart
}

pub type Level {
  None
  Verbose
  Trace
  Driver
  Integer(inner: Int)
}

@external(erlang, "wx", "new")
fn external_new_with(options: List(NewOption)) -> WxObject

@external(erlang, "wx", "new")
fn external_new() -> WxObject

fn new(options: List(NewOption)) -> WxObject {
  case options {
    [] -> external_new()
    v -> external_new_with(v)
  }
}

@external(erlang, "wx", "destroy")
fn destroy() -> Nil

pub fn start() -> Result(Nil, Nil) {
  case ensure_all_started("wx" |> atom.create()) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(Nil)
  }
}

pub fn handle_app(options: List(NewOption), mainloop: fn (WxObject) -> Nil) -> Nil {
  let wx_object: WxObject = new(options)
  mainloop(wx_object)
  destroy()
}
