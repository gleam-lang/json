import gleam/dynamic
import gleam/option.{None, Some}
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

pub fn encode_test() {
  json.string("hello")
  |> json.encode()
  |> should.equal("\"hello\"")

  json.null()
  |> json.encode()
  |> should.equal("null")

  json.object([tuple("foo", json.int(5))])
  |> json.encode()
  |> should.equal("{\"foo\":5}")

  json.nullable(Some(5), json.int)
  |> json.encode()
  |> should.equal("5")

  json.nullable(None, json.int)
  |> json.encode()
  |> should.equal("null")
}
