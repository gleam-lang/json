import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string_builder.{type StringBuilder}

pub type Json

pub type DecodeError {
  UnexpectedEndOfInput
  UnexpectedByte(String)
  UnexpectedSequence(String)
  UnexpectedFormat(List(dynamic.DecodeError))
}

/// Decode a JSON string into dynamically typed data which can be decoded into
/// typed data with the `gleam/dynamic` module.
///
/// ## Examples
///
/// ```gleam
/// > decode("[1,2,3]", dynamic.list(of: dynamic.int))
/// Ok([1, 2, 3])
/// ```
///
/// ```gleam
/// > decode("[", dynamic.list(of: dynamic.int))
/// Error(UnexpectedEndOfInput)
/// ```
///
/// ```gleam
/// > decode("1", dynamic.string)
/// Error(UnexpectedFormat([dynamic.DecodeError("String", "Int", [])]))
/// ```
///
pub fn decode(
  from json: String,
  using decoder: dynamic.Decoder(t),
) -> Result(t, DecodeError) {
  do_decode(from: json, using: decoder)
}

@target(erlang)
fn do_decode(
  from json: String,
  using decoder: dynamic.Decoder(t),
) -> Result(t, DecodeError) {
  let bits = bit_array.from_string(json)
  decode_bits(bits, decoder)
}

@target(javascript)
fn do_decode(
  from json: String,
  using decoder: dynamic.Decoder(t),
) -> Result(t, DecodeError) {
  use dynamic_value <- result.then(decode_string(json))
  decoder(dynamic_value)
  |> result.map_error(UnexpectedFormat)
}

@target(javascript)
@external(javascript, "../gleam_json_ffi.mjs", "decode")
fn decode_string(a: String) -> Result(Dynamic, DecodeError)

/// Decode a JSON bit string into dynamically typed data which can be decoded
/// into typed data with the `gleam/dynamic` module.
///
/// ## Examples
///
/// ```gleam
/// > decode_bits(<<"[1,2,3]">>, dynamic.list(of: dynamic.int))
/// Ok([1, 2, 3])
/// ```
///
/// ```gleam
/// > decode_bits(<<"[">>, dynamic.list(of: dynamic.int))
/// Error(UnexpectedEndOfInput)
/// ```
///
/// ```gleam
/// > decode_bits("<<1">>, dynamic.string)
/// Error(UnexpectedFormat([dynamic.DecodeError("String", "Int", [])]))
/// ```
///
pub fn decode_bits(
  from json: BitArray,
  using decoder: dynamic.Decoder(t),
) -> Result(t, DecodeError) {
  use dynamic_value <- result.then(decode_to_dynamic(json))
  decoder(dynamic_value)
  |> result.map_error(UnexpectedFormat)
}

@target(erlang)
@external(erlang, "gleam_json_ffi", "decode")
fn decode_to_dynamic(a: BitArray) -> Result(Dynamic, DecodeError)

@target(javascript)
fn decode_to_dynamic(json: BitArray) -> Result(Dynamic, DecodeError) {
  case bit_array.to_string(json) {
    Ok(string) -> decode_string(string)
    Error(Nil) -> Error(UnexpectedByte(""))
  }
}

/// Convert a JSON value into a string.
///
/// Where possible prefer the `to_string_builder` function as it is faster than
/// this function, and BEAM VM IO is optimised for sending `StringBuilder` data.
///
/// ## Examples
///
/// ```gleam
/// > to_string(array([1, 2, 3], of: int))
/// "[1,2,3]"
/// ```
///
pub fn to_string(json: Json) -> String {
  do_to_string(json)
}

@external(erlang, "gleam_json_ffi", "json_to_string")
@external(javascript, "../gleam_json_ffi.mjs", "json_to_string")
fn do_to_string(a: Json) -> String

/// Convert a JSON value into a string builder.
///
/// Where possible prefer this function to the `to_string` function as it is
/// slower than this function, and BEAM VM IO is optimised for sending
/// `StringBuilder` data.
///
/// ## Examples
///
/// ```gleam
/// > to_string_builder(array([1, 2, 3], of: int))
/// string_builder.from_string("[1,2,3]")
/// ```
///
pub fn to_string_builder(json: Json) -> StringBuilder {
  do_to_string_builder(json)
}

@external(erlang, "gleam_json_ffi", "json_to_iodata")
@external(javascript, "../gleam_json_ffi.mjs", "json_to_string")
fn do_to_string_builder(a: Json) -> StringBuilder

/// Encode a string into JSON, using normal JSON escaping.
///
/// ## Examples
///
/// ```gleam
/// > to_string(string("Hello!"))
/// "\"Hello!\""
/// ```
///
pub fn string(input: String) -> Json {
  do_string(input)
}

@external(erlang, "gleam_json_ffi", "string")
@external(javascript, "../gleam_json_ffi.mjs", "identity")
fn do_string(a: String) -> Json

/// Encode a bool into JSON.
///
/// ## Examples
///
/// ```gleam
/// > to_string(bool(False))
/// "false"
/// ```
///
pub fn bool(input: Bool) -> Json {
  do_bool(input)
}

@external(erlang, "gleam_json_ffi", "bool")
@external(javascript, "../gleam_json_ffi.mjs", "identity")
fn do_bool(a: Bool) -> Json

/// Encode an int into JSON.
///
/// ## Examples
///
/// ```gleam
/// > to_string(int(50))
/// "50"
/// ```
///
pub fn int(input: Int) -> Json {
  do_int(input)
}

@external(erlang, "gleam_json_ffi", "int")
@external(javascript, "../gleam_json_ffi.mjs", "identity")
fn do_int(a: Int) -> Json

/// Encode a float into JSON.
///
/// ## Examples
///
/// ```gleam
/// > to_string(float(4.7))
/// "4.7"
/// ```
///
pub fn float(input: Float) -> Json {
  do_float(input)
}

@external(erlang, "gleam_json_ffi", "float")
@external(javascript, "../gleam_json_ffi.mjs", "identity")
fn do_float(input input: Float) -> Json

/// The JSON value null.
///
/// ## Examples
///
/// ```gleam
/// > to_string(null())
/// "null"
/// ```
///
pub fn null() -> Json {
  do_null()
}

@external(erlang, "gleam_json_ffi", "null")
@external(javascript, "../gleam_json_ffi.mjs", "do_null")
fn do_null() -> Json

/// Encode an optional value into JSON, using null if it is the `None` variant.
///
/// ## Examples
///
/// ```gleam
/// > to_string(nullable(Some(50), of: int))
/// "50"
/// ```
///
/// ```gleam
/// > to_string(nullable(None, of: int))
/// "null"
/// ```
///
pub fn nullable(from input: Option(a), of inner_type: fn(a) -> Json) -> Json {
  case input {
    Some(value) -> inner_type(value)
    None -> null()
  }
}

/// Encode a list of key-value pairs into a JSON object.
///
/// ## Examples
///
/// ```gleam
/// > to_string(object([
///   #("game", string("Pac-Man")),
///   #("score", int(3333360)),
/// ]))
/// "{\"game\":\"Pac-Mac\",\"score\":3333360}"
/// ```
///
pub fn object(entries: List(#(String, Json))) -> Json {
  do_object(entries)
}

@external(erlang, "gleam_json_ffi", "object")
@external(javascript, "../gleam_json_ffi.mjs", "object")
fn do_object(entries entries: List(#(String, Json))) -> Json

/// Encode a list into a JSON array.
///
/// ## Examples
///
/// ```gleam
/// > to_string(array([1, 2, 3], of: int))
/// "[1, 2, 3]"
/// ```
///
pub fn array(from entries: List(a), of inner_type: fn(a) -> Json) -> Json {
  entries
  |> list.map(inner_type)
  |> preprocessed_array
}

/// Encode a list of JSON values into a JSON array.
///
/// ## Examples
///
/// ```gleam
/// > to_string(preprocessed_array([int(1), float(2.0), string("3")]))
/// "[1, 2.0, \"3\"]"
/// ```
///
pub fn preprocessed_array(from: List(Json)) -> Json {
  do_preprocessed_array(from)
}

@external(erlang, "gleam_json_ffi", "array")
@external(javascript, "../gleam_json_ffi.mjs", "array")
fn do_preprocessed_array(from from: List(Json)) -> Json
