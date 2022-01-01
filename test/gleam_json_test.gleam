import gleam/dynamic
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn decode_test() {
  json.decode("5")
  |> result.map(dynamic.from)
  |> should.equal(Ok(dynamic.from(5)))
}

pub fn decode_empty_test() {
  json.decode("")
  |> should.equal(Error(json.UnexpectedEndOfInput))
}

pub fn decode_unexpected_byte_test() {
  json.decode("[}")
  |> should.equal(Error(json.UnexpectedByte("0x7D", 1)))
}

pub fn encode_string_test() {
  json.string("hello")
  |> json.to_string()
  |> should.equal("\"hello\"")
}

pub fn encode_null_test() {
  json.null()
  |> json.to_string()
  |> should.equal("null")
}

pub fn encode_object_test() {
  json.object([#("foo", json.int(5))])
  |> json.to_string()
  |> should.equal("{\"foo\":5}")
}

pub fn encode_list_test() {
  json.list([json.int(5), json.int(6)])
  |> json.to_string()
  |> should.equal("[5,6]")
}

pub fn encode_nullable_test() {
  json.nullable(Some(5), json.int)
  |> json.to_string()
  |> should.equal("5")

  json.nullable(None, json.int)
  |> json.to_string()
  |> should.equal("null")
}
