import gleam/erlang/atom
import gleeunit/should
import wxgleam/internal

pub fn null_test() {
  let wx_null_object: internal.WxObject = internal.null()

  wx_null_object |> internal.is_null() |> should.be_true()
  wx_null_object |> internal.equal(internal.null()) |> should.be_true()
  wx_null_object
  |> internal.get_object_type()
  |> should.equal(atom.create("wx"))
}

pub fn new_test() {
  let wx_new_object: internal.WxObject = internal.new([])

  wx_new_object |> internal.is_null() |> should.be_true()
  wx_new_object |> internal.equal(internal.null()) |> should.be_true()
  wx_new_object
  |> internal.get_object_type()
  |> should.equal(atom.create("wx"))
}
