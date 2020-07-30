import gleam/dynamic.{Dynamic}


pub external type Json

pub type Failure {
  Badarg(Dynamic)
}

pub type DecodeResult {
  Ok(Json, String)
  Error(Failure)
}
