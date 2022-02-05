# Changelog

## v0.4.0 - Unreleased

- The `decode_bits` function has been added.

## v0.3.0 - 2022-01-09

- The `decode` function now takes a `gleam/dynamic.Decoder`.

## v0.2.0 - 2022-01-01

- Converted to use the Gleam build tool.
- The underlying `jsone` Erlang JSON libary has been replaced with the new
  `thoas` Erlang JSON library.
- The `encode` function has been replaced by the `to_string` and
  `to_string_builder` functions.
- The `list` function has been replaced by the `array` and `preprocessed_array`
  functions.
- The `nullable` function gains argument labels.
- The `float` function has been added.

## v0.1.0 - 2020-07-30

- Initial release.
