import gleam/dynamic.{Dynamic}
import gleam/result
import gleam/json/types

pub type Json = types.Json

external fn jsone_decode(String) -> types.DecodeResult =
  "jsone_decode" "decode"

pub fn decode(encoded: String) -> Result(Json, Dynamic) {
  case jsone_decode(encoded) {
    types.Ok(json, _rest) -> Ok(json)
    types.Error(types.Badarg(reason)) -> Error(reason)
  }
}
