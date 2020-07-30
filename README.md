# gleam_json

A Gleam library for encoding decoding JSON.

### Encoding

```rust
import gleam/json

let data = json.object([
  tuple("foo", json.string("bar")),
  tuple("values", json.list([json.int(1), json.int(2)])),
  tuple("nope", json.null())
])
json.encode(data)
// {"foo":"bar","nope":null,"values":[1,2]}
```

### Decoding

Note: JSON is a dynamic data structure.
The best way to manage decode JSON data is using `gleam/dynamic`.

```rust
import gleam/json

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

This package can be installed by adding `gleam_json` to your `rebar.config` dependencies:

```erlang
{deps, [
    gleam_json
]}.
```
