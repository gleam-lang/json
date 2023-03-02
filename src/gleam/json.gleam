import gleam/list
import gleam/result
import gleam/bit_string
import gleam/option.{None, Option, Some}
import gleam/dynamic.{Dynamic}
import gleam/string_builder.{StringBuilder}

pub external type Json

pub type DecodeError {
  UnexpectedEndOfInput
  UnexpectedByte(byte: String, position: Int)
  UnexpectedSequence(byte: String, position: Int)
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
/// > decode("[", into: dynamic.list(of: dynamic.int))
/// Error(UnexpectedEndOfInput)
/// ```
///
/// ```gleam
/// > decode("1", into: dynamic.string)
/// Error(UnexpectedFormat([dynamic.DecodeError("String", "Int", [])]))
/// ```
///
pub fn decode(
  from json: String,
  using decoder: dynamic.Decoder(t),
) -> Result(t, DecodeError) {
  do_decode(from: json, using: decoder)
}

if erlang {
  fn do_decode(
    from json: String,
    using decoder: dynamic.Decoder(t),
  ) -> Result(t, DecodeError) {
    let bits = bit_string.from_string(json)
    decode_bits(bits, decoder)
  }
}

if javascript {
  fn do_decode(
    from json: String,
    using decoder: dynamic.Decoder(t),
  ) -> Result(t, DecodeError) {
    use dynamic_value <- result.then(decode_string(json))
    decoder(dynamic_value)
    |> result.map_error(UnexpectedFormat)
  }

  external fn decode_string(String) -> Result(Dynamic, DecodeError) =
    "../gleam_json_ffi.mjs" "decode"
}

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
/// > decode_bits(<<"[">>, into: dynamic.list(of: dynamic.int))
/// Error(UnexpectedEndOfInput)
/// ```
///
/// ```gleam
/// > decode_bits("<<1">>, into: dynamic.string)
/// Error(UnexpectedFormat([dynamic.DecodeError("String", "Int", [])]))
/// ```
///
pub fn decode_bits(
  from json: BitString,
  using decoder: dynamic.Decoder(t),
) -> Result(t, DecodeError) {
  use dynamic_value <- result.then(decode_to_dynamic(json))
  decoder(dynamic_value)
  |> result.map_error(UnexpectedFormat)
}

if erlang {
  external fn decode_to_dynamic(BitString) -> Result(Dynamic, DecodeError) =
    "gleam_json_ffi" "decode"
}

if javascript {
  fn decode_to_dynamic(json: BitString) -> Result(Dynamic, DecodeError) {
    case bit_string.to_string(json) {
      Ok(string) -> decode_string(string)
      Error(Nil) -> Error(UnexpectedByte("", 0))
    }
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

if erlang {
  external fn do_to_string(Json) -> String =
    "gleam_json_ffi" "json_to_string"
}

if javascript {
  external fn do_to_string(Json) -> String =
    "../gleam_json_ffi.mjs" "json_to_string"
}

/// Convert a JSON value into a string builder.
///
/// Where possible prefer this function to the  `to_string` function as it is
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

if erlang {
  external fn do_to_string_builder(Json) -> StringBuilder =
    "gleam_json_ffi" "json_to_iodata"
}

if javascript {
  external fn do_to_string_builder(Json) -> StringBuilder =
    "../gleam_json_ffi.mjs" "json_to_string"
}

/// Encode a string into JSON, using normal JSON escaping.
///
/// ## Examples
///
/// ```gleam
/// > to_string("Hello!")
/// "\"Hello!\""
/// ```
///
pub fn string(input: String) -> Json {
  do_string(input)
}

if erlang {
  external fn do_string(String) -> Json =
    "gleam_json_ffi" "string"
}

if javascript {
  external fn do_string(String) -> Json =
    "../gleam_json_ffi.mjs" "identity"
}

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

if erlang {
  external fn do_bool(Bool) -> Json =
    "gleam_json_ffi" "bool"
}

if javascript {
  external fn do_bool(Bool) -> Json =
    "../gleam_json_ffi.mjs" "identity"
}

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

if erlang {
  external fn do_int(Int) -> Json =
    "gleam_json_ffi" "int"
}

if javascript {
  external fn do_int(Int) -> Json =
    "../gleam_json_ffi.mjs" "identity"
}

/// Encode an float into JSON.
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

if erlang {
  external fn do_float(input: Float) -> Json =
    "gleam_json_ffi" "float"
}

if javascript {
  external fn do_float(input: Float) -> Json =
    "../gleam_json_ffi.mjs" "identity"
}

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

if erlang {
  external fn do_null() -> Json =
    "gleam_json_ffi" "null"
}

if javascript {
  external fn do_null() -> Json =
    "../gleam_json_ffi.mjs" "do_null"
}

/// Encode an optional value into JSON, using null if it the `None` variant.
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

if erlang {
  external fn do_object(entries: List(#(String, Json))) -> Json =
    "gleam_json_ffi" "object"
}

if javascript {
  external fn do_object(entries: List(#(String, Json))) -> Json =
    "../gleam_json_ffi.mjs" "object"
}

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

if erlang {
  external fn do_preprocessed_array(from: List(Json)) -> Json =
    "gleam_json_ffi" "array"
}

if javascript {
  external fn do_preprocessed_array(from: List(Json)) -> Json =
    "../gleam_json_ffi.mjs" "array"
}
