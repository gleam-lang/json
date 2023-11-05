import gleam/dynamic
import gleam/json.{type Json}
import gleam/option.{None, Some}
import gleam/string_builder
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn decode_test() {
  json.decode(from: "5", using: dynamic.int)
  |> should.equal(Ok(5))
}

pub fn decode_empty_test() {
  json.decode(from: "", using: dynamic.int)
  |> should.equal(Error(json.UnexpectedEndOfInput))
}

pub fn decode_unexpected_byte_test() {
  let assert Error(error) = json.decode(from: "[}", using: dynamic.int)
  let assert json.UnexpectedByte(byte, index) = error
  let assert "0x7D" = byte

  // V8 does not report the position of the unexpected byte any more.
  let assert True = index == 1 || index == -1
}

pub fn decode_unexpected_format_test() {
  json.decode(from: "[]", using: dynamic.int)
  |> should.equal(Error(json.UnexpectedFormat([empty_list_decode_error()])))
}

pub fn decode_bits_test() {
  json.decode_bits(from: <<"5":utf8>>, using: dynamic.int)
  |> should.equal(Ok(5))
}

pub fn decode_bits_empty_test() {
  json.decode_bits(from: <<"":utf8>>, using: dynamic.int)
  |> should.equal(Error(json.UnexpectedEndOfInput))
}

pub fn decode_bits_unexpected_byte_test() {
  let assert Error(error) = json.decode(from: "[}", using: dynamic.int)
  let assert json.UnexpectedByte(byte, index) = error
  let assert "0x7D" = byte

  // V8 does not report the position of the unexpected byte any more.
  let assert True = index == 1 || index == -1
}

pub fn decode_bits_unexpected_format_test() {
  json.decode_bits(from: <<"[]":utf8>>, using: dynamic.int)
  |> should.equal(Error(json.UnexpectedFormat([empty_list_decode_error()])))
}

pub fn encode_string_test() {
  json.string("hello")
  |> should_encode("\"hello\"")
}

pub fn encode_null_test() {
  json.null()
  |> should_encode("null")
}

pub fn encode_int_test() {
  json.int(-50)
  |> should_encode("-50")

  json.int(100)
  |> should_encode("100")
}

pub fn encode_float_test() {
  json.float(-50.5)
  |> should_encode("-50.5")

  json.float(100.1)
  |> should_encode("100.1")
}

pub fn encode_object_test() {
  json.object([#("foo", json.int(5))])
  |> should_encode("{\"foo\":5}")
}

pub fn encode_array_test() {
  [5, 6, 1, 4]
  |> json.array(of: json.int)
  |> should_encode("[5,6,1,4]")
}

pub fn encode_preprocessed_array_test() {
  json.preprocessed_array([json.int(5), json.int(6)])
  |> should_encode("[5,6]")
}

pub fn encode_nullable_some_test() {
  json.nullable(Some(5), of: json.int)
  |> should_encode("5")
}

pub fn encode_nullable_none_test() {
  json.nullable(None, of: json.int)
  |> should_encode("null")
}

pub fn encode_bool_true_test() {
  json.bool(True)
  |> should_encode("true")
}

pub fn encode_bool_false_test() {
  json.bool(False)
  |> should_encode("false")
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

@target(erlang)
fn empty_list_decode_error() -> dynamic.DecodeError {
  dynamic.DecodeError(expected: "Int", found: "List", path: [])
}

@target(javascript)
fn empty_list_decode_error() {
  dynamic.DecodeError(expected: "Int", found: "Tuple of 0 elements", path: [])
}
