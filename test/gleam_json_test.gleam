import gleam_json
import gleam/should

pub fn hello_world_test() {
  gleam_json.hello_world()
  |> should.equal("Hello, from gleam_json!")
}
