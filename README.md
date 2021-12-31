# json 🐑

Work with JSON in Gleam!

### Encoding

```rust
import gleam/json.{object, string, list, int, null}

let data = object([
  #("foo", string("bar")),
  #("values", list([int(1), int(2)])),
  #("nope", null())
])
json.encode(data)
// {"foo":"bar","nope":null,"values":[1,2]}
```

### Decoding

Note: JSON is a dynamic data structure.
The best way to manage decode JSON data is using `gleam/dynamic`.

```rust
import gleam/json
import gleam/dynamic

let encoded =
  "{\"foo\":\"bar\",\"nope\":null,\"values\":[1,2]}"

assert Ok(data) = json.decode(encoded)
let data = dynamic.from(data)

assert Ok(foo) = dynamic.field(data, "foo")
assert Ok("bar") = dynamic.string(foo)

assert Ok(values) = dynamic.field(data, "values")
assert Ok([1, 2]) = dynamic.typed_list(values, dynamic.int)

assert Ok(nope) = dynamic.field(data, "nope")
assert Ok(_) = dynamic.atom(nope)
```

## Installation

Add this package to your Gleam project.

```shell
gleam add gleam_json
```
