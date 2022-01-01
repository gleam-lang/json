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

  json.decode(".")
  |> result.nil_error()
  |> should.equal(Error(Nil))
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

pub fn encode_nullable_test() {
  json.nullable(Some(5), json.int)
  |> json.to_string()
  |> should.equal("5")

  json.nullable(None, json.int)
  |> json.to_string()
  |> should.equal("null")
}
