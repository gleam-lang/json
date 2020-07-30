import gleam/dynamic.{Dynamic}
import gleam/map
import gleam/option.{Option}
import gleam/result
import gleam/json/types

pub type Json =
  types.Json

external fn jsone_decode(String) -> types.DecodeResult =
  "jsone_decode" "decode"

pub fn decode(encoded: String) -> Result(Json, Dynamic) {
  case jsone_decode(encoded) {
    types.Ok(json, _rest) -> Ok(json)
    types.Error(types.Badarg(reason)) -> Error(reason)
  }
}

external fn jsone_encode(Json) -> Result(String, types.DecodeResult) =
  "jsone_encode" "encode"

pub fn encode(json: Json) -> String {
  // The encoder only error if input is invalid, i.e. a PID.
  // This cannot happen when passing in the the Json type.
  assert Ok(encoded) = jsone_encode(json)
  encoded
}

pub fn string(input: String) -> Json {
  input
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn bool(input: Bool) -> Json {
  input
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn int(input: Int) -> Json {
  input
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

type Null {
  Null
}

pub fn null() -> Json {
  Null
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn nullable(input: Option(a), mapper: fn(a) -> Json) -> Json {
  input
  |> option.map(mapper)
  |> option.unwrap(null())
}

pub fn object(entries: List(tuple(String, Json))) -> Json {
  entries
  |> map.from_list()
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}

pub fn list(input: List(Json)) -> Json {
  input
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
}
