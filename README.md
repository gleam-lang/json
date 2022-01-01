# json 🐑

Work with JSON in Gleam!

Under the hood library uses [Thoas](https://github.com/lpil/thoas/), the fastest
and most memory efficient pure Erlang JSON encoder/decoder.

## Installation

Add this package to your Gleam project.

```shell
gleam add gleam_json
```

### Encoding

```rust
import myapp.{Cat}
import gleam/list
import gleam/json.{object, string, list, int, null}

pub fn cat_to_json(cat: Cat) -> String {
  object([
    #("name", string(cat.name)),
    #("lives", int(9),
    #("flaws", null()),
    #("nicknames", array(["Kitty", "Sweetie"], of: string)),
  ])
  |> json.to_string
}
```

### Decoding

JSON is decoded into a `Dynamic` value which can be decoded using the
`gleam/dynamic` module from the Gleam standard library.

```rust
import myapp.{Cat}
import gleam/json
import gleam/dynamic
import gleam/result

pub fn cat_from_json(json: String) -> Result<Cat, MyError> {
  try data = 
    json.decode(encoded)
    |> result.map_error(InvalidJson)

  let data = dynamic.from(data)
  try cat = {
    try name = dynamic.field(data, "name")
    try name = dynamic.string(name)
    Ok(Cat(name))
  }
  |> result.map_error(InvalidFormat)

  Ok(cat)
}

pub type MyError {
  InvalidJson(json.DecodeError)
  InvalidFormat(dynamic.DecodeError)
}
```
