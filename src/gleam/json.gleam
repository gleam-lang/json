import gleam/map
import gleam/list
import gleam/result
import gleam/option.{None, Option, Some}
import gleam/dynamic.{Dynamic}
import gleam/string_builder.{StringBuilder}

pub external type Json

pub type DecodeError {
  UnexpectedEndOfInput
  UnexpectedByte(byte: String, position: Int)
  UnexpectedSequence(byte: String, position: Int)
}

/// Decode a JSON string into dynamically typed data which can be decoded into
/// typed data with the `gleam/dynamic` module.
///
/// ## Examples
///
/// ```gleam
/// > decode("[1,2,3]")
/// Ok(dynamic.from([1, 2, 3]))
/// ```
///
/// ```gleam
/// > decode("[")
/// Error(UnexpectedEndOfInput)
/// ```
///
pub external fn decode(String) -> Result(Dynamic, DecodeError) =
  "thoas" "decode"

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
pub external fn to_string(Json) -> String =
  "gleam_json_ffi" "json_to_string"

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
pub external fn to_string_builder(Json) -> StringBuilder =
  "gleam_json_ffi" "json_to_iodata"

/// Encode a string into JSON, using normal JSON escaping.
///
/// ## Examples
///
/// ```gleam
/// > to_string("Hello!")
/// "\"Hello!\""
/// ```
///
pub external fn string(input: String) -> Json =
  "thoas_encode" "string"

/// Encode a bool into JSON.
///
/// ## Examples
///
/// ```gleam
/// > to_string(bool(False))
/// "false"
/// ```
///
pub external fn bool(input: Bool) -> Json =
  "thoas_encode" "boolean"

/// Encode an int into JSON.
///
/// ## Examples
///
/// ```gleam
/// > to_string(int(50))
/// "50"
/// ```
///
pub external fn int(input: Int) -> Json =
  "thoas_encode" "integer"

/// The JSON value null.
///
/// ## Examples
///
/// ```gleam
/// > to_string(null())
/// "null"
/// ```
///
pub external fn null() -> Json =
  "thoas_encode" "null"

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
pub external fn object(entries: List(#(String, Json))) -> Json =
  "thoas_encode" "non_recursive_object"

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
pub external fn preprocessed_array(from: List(Json)) -> Json =
  "thoas_encode" "non_recursive_array"
