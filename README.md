# json 🐑

Work with JSON in Gleam!

## Installation

Erlang/OTP 27.0 or higher is required when targeting Erlang.
To use earlier versions of Erlang/OTP use version 1.0.1 of
this package.

Add this package to your Gleam project.

```shell
# Erlang version <= OTP26
gleam add gleam_json@1

# Erlang version >= OTP27
gleam add gleam_json@2
```

## Encoding

```gleam
import myapp.{type Cat}
import gleam/json.{object, string, array, int, null}

pub fn cat_to_json(cat: Cat) -> String {
  object([
    #("name", string(cat.name)),
    #("lives", int(cat.lives)),
    #("flaws", null()),
    #("nicknames", array(cat.nicknames, of: string)),
  ])
  |> json.to_string
}
```

## Decoding

JSON is decoded into a `Dynamic` value which can be decoded using the
`gleam/dynamic` module from the Gleam standard library.

```gleam
import myapp.{Cat}
import gleam/json
import gleam/dynamic.{field, list, int, string}

pub fn cat_from_json(json_string: String) -> Result(Cat, json.DecodeError) {
  let cat_decoder = dynamic.decode3(
    Cat,
    field("name", of: string),
    field("lives", of: int),
    field("nicknames", of: list(string)),
  )

  json.decode(from: json_string, using: cat_decoder)
}
```
