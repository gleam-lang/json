@target(javascript)
import gleam/json.{DecodeError, UnexpectedByte, UnexpectedEndOfInput}
@target(javascript)
import gleeunit/should

@target(javascript)
type StandardError {
  StandardError(message: String)
}

// === End of input tests === //
@target(javascript)
pub fn chromium_end_of_input_test() {
  "Unexpected end of JSON input"
  |> StandardError
  |> get_json_decode_error("")
  |> should.equal(UnexpectedEndOfInput)
}

@target(javascript)
pub fn spidermonkey_end_of_input_test() {
  "JSON.parse: unexpected end of data at line 1 column 1 of the JSON data"
  |> StandardError
  |> get_json_decode_error("")
  |> should.equal(UnexpectedEndOfInput)
}

@target(javascript)
pub fn javascript_core_end_of_input_test() {
  "JSON Parse error: Unexpected EOF"
  |> StandardError
  |> get_json_decode_error("")
  |> should.equal(UnexpectedEndOfInput)
}

// === Unexpected byte tests === //
@target(javascript)
pub fn chromium_unexpected_byte_test() {
  "Unexpected token a in JSON at position 5"
  |> StandardError
  |> get_json_decode_error("{\"b\":a}")
  |> should.equal(UnexpectedByte(byte: "0x61", position: 5))
}

@target(javascript)
pub fn spidermonkey_unexpected_byte_test() {
  "JSON.parse: expected property name or '}' at line 1 column 6 of the JSON data"
  |> StandardError
  |> get_json_decode_error("{\"b\":a}")
  |> should.equal(UnexpectedByte(byte: "0x61", position: 5))
}

@target(javascript)
pub fn javascript_core_unexpected_byte_test() {
  "JSON Parse error: Unexpected identifier \"a\""
  |> StandardError
  |> get_json_decode_error("{\"b\":a}")
  |> should.equal(UnexpectedByte(byte: "0x61", position: 0))
}

@target(javascript)
pub fn spidermonkey_multiline_unexpected_byte_test() {
  "JSON.parse: expected property name or '}' at line 2 column 6 of the JSON data"
  |> StandardError
  |> get_json_decode_error("{\n\"b\": a\n}")
  |> should.equal(UnexpectedByte(byte: "0x61", position: 7))

  "JSON.parse: expected double-quoted property name at line 3 column 1 of the JSON data"
  |> StandardError
  |> get_json_decode_error("{\n\"b\": \"x\",\na\n}")
  |> should.equal(UnexpectedByte(byte: "0x61", position: 12))
}

@target(javascript)
@external(javascript, "./gleam_json_ffi.mjs", "getJsonDecodeError")
fn get_json_decode_error(a: StandardError, b: String) -> DecodeError
