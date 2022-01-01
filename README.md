# json 🐑

Work with JSON in Gleam!

### Encoding

```rust
import gleam/list
import gleam/json.{object, string, list, int, null}

pub type Cat {
  Cat(name: String)
}

pub fn cat_to_json(cat: Cat) -> String {
  object([
    #("name", string(cat.name)),
    #("lives", int(9),
    #("flaws", null()),
    #("nicknames", list([string("Kitty"), string("Sweetie")])),
  ])
  |> json.to_string
}
```

### Decoding

JSON is decoded into a `Dynamic` value which can be decoded using the
`gleam/dynamic` module from the Gleam standard library.

```rust
import gleam/json
import gleam/dynamic.{DecodeError}
import gleam/result

pub type MyError {
  InvalidJson
  InvalidFormat(DecodeError)
}

pub fn cat_from_json(json: String) -> Result<Cat, MyError> {
  try data = 
    json.decode(encoded)
    |> result.put_error(InvalidJson)

  let data = dynamic.from(data)
  try cat = {
    try name = dynamic.field(data, "name")
    try name = dynamic.string(name)
    Ok(Cat(name))
  }
  |> result.map_error(InvalidFormat)

  Ok(cat)
}
```

## Installation

Add this package to your Gleam project.

```shell
gleam add gleam_json
```
