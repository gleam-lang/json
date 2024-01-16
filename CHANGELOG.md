# Changelog

## v1.0.0 - 2024-01-16

- Relaxed stdlib version requirement.

## v0.7.0 - 2023-11-05

- Updated for Gleam v0.32.0.

## v0.6.0 - 2023-08-03

- Updated error parsing for current V8 runtimes.
- Updated for Gleam v0.30.0.

## v0.5.1 - 2023-03-02

- Updated for Gleam v0.27.0.

## v0.5.0 - 2022-06-11

- This library now works when running Gleam on JavaScript.

## v0.4.0 - 2022-02-05

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
