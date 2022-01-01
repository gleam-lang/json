import gleam/dynamic
import gleam/json.{Json}
import gleam/option.{None, Some}
import gleam/string_builder
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
  |> should_encode("\"hello\"")
}

pub fn encode_null_test() {
  json.null()
  |> should_encode("null")
}

pub fn encode_object_test() {
  json.object([#("foo", json.int(5))])
  |> should_encode("{\"foo\":5}")
}

pub fn encode_list_test() {
  json.list([json.int(5), json.int(6)])
  |> should_encode("[5,6]")
}

pub fn encode_nullable_some_test() {
  json.nullable(Some(5), json.int)
  |> should_encode("5")
}

pub fn encode_nullable_none_test() {
  json.nullable(None, json.int)
  |> should_encode("null")
}

fn should_encode(data: Json, expected: String) {
  data
  |> json.to_string()
  |> should.equal(expected)

  data
  |> json.to_string_builder
  |> string_builder.to_string
  |> should.equal(json.to_string(data))
}
