import gleam/option.{None}
import gleeunit
import gleeunit/should
import wxgleam/internal

pub fn null_test() {
  let wx_null_object: internal.WxObject = internal.null()
  wx_null_object |> internal.is_null() |> should.be_true()
}
