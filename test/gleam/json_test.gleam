import gleam/dynamic
import gleam/result
import gleam/json.{Json}
import gleam/should

pub fn decode_test() {
  json.decode("5")
  |> result.map(dynamic.from)
  |> should.equal(Ok(dynamic.from(5)))

  json.decode(".")
  |> result.nil_error()
  |> should.equal(Error(Nil))
}
