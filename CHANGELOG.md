# Changelog

## Unreleased

- Converted to use the Gleam build tool.
- The underlying `jsone` Erlang JSON libary has been replaced with the new
  `thoas` Erlang JSON library.
- The `encode` function has been replaced by the `to_string` and
  `to_string_builder` functions.
- The `list` function has been replaced by the `array` and `preprocessed_array`
  functions.
- The `nullable` function gains argument labels.

## v0.1.0 - 2020-07-30

- Initial release.
