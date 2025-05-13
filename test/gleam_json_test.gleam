import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json.{type Json}
import gleam/option.{None, Some}
import gleam/string
import gleam/string_tree
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

@target(erlang)
const list_found = "List"

@target(javascript)
const list_found = "Array"

pub fn parse_test() {
  json.parse(from: "5", using: decode.int)
  |> should.equal(Ok(5))
}

pub fn parse_empty_test() {
  json.parse(from: "", using: decode.int)
  |> should.equal(Error(json.UnexpectedEndOfInput))
}

pub fn parse_unexpected_byte_test() {
  let assert Error(error) = json.parse(from: "[}", using: decode.int)
  let assert json.UnexpectedByte(byte) = error
  let assert "0x7D" = byte
}

pub fn parse_unexpected_format_test() {
  json.parse(from: "[]", using: decode.int)
  |> should.equal(
    Error(
      json.UnableToDecode([
        decode.DecodeError(expected: "Int", found: list_found, path: []),
      ]),
    ),
  )
}

pub fn parse_unable_to_decode_test() {
  json.parse(from: "[]", using: decode.int)
  |> should.equal(
    Error(
      json.UnableToDecode([
        decode.DecodeError(expected: "Int", found: list_found, path: []),
      ]),
    ),
  )
}

pub fn parse_bits_test() {
  json.parse_bits(from: <<"5":utf8>>, using: decode.int)
  |> should.equal(Ok(5))
}

pub fn parse_bits_empty_test() {
  json.parse_bits(from: <<"":utf8>>, using: decode.int)
  |> should.equal(Error(json.UnexpectedEndOfInput))
}

pub fn parse_bits_unexpected_byte_test() {
  let assert Error(error) = json.parse(from: "[}", using: decode.int)
  let assert json.UnexpectedByte(byte) = error
  let assert "0x7D" = byte
}

pub fn parse_bits_unexpected_format_test() {
  json.parse_bits(from: <<"[]":utf8>>, using: decode.int)
  |> should.equal(
    Error(
      json.UnableToDecode([
        decode.DecodeError(expected: "Int", found: list_found, path: []),
      ]),
    ),
  )
}

pub fn parse_unexpected_sequence_test() {
  let assert Error(error) = json.parse(from: "\"\\uxxxx\"", using: decode.float)
  case error {
    json.UnexpectedSequence("\\uxxxx") -> Nil
    json.UnexpectedByte("0x78") -> Nil
    _ -> panic as { "unexpected error: " <> string.inspect(error) }
  }
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

pub fn encode_empty_object_test() {
  json.object([])
  |> should_encode("{}")
}

pub fn encode_empty_array_test() {
  []
  |> json.array(of: json.int)
  |> should_encode("[]")
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

pub fn encode_empty_preprocessed_array_test() {
  json.preprocessed_array([])
  |> should_encode("[]")
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

pub fn encode_dict_test() {
  json.dict(dict.from_list([#(3, 3)]), int.to_string, json.int)
  |> should_encode("{\"3\":3}")
}

fn should_encode(data: Json, expected: String) {
  data
  |> json.to_string()
  |> should.equal(expected)

  data
  |> json.to_string_tree
  |> string_tree.to_string
  |> should.equal(json.to_string(data))
}
