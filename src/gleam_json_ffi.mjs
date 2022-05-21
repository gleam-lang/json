import { Ok, Error, CustomType } from './gleam.mjs'
import { bit_string_to_string } from '../../gleam_stdlib/dist/gleam_stdlib.mjs'

export class DecodeError extends CustomType {
  get __gleam_prelude_variant__() {
    return "DecodeError";
  }
}

export class UnexpectedEndOfInput extends CustomType {
  static isInstance(err) {
    return err.message === 'Unexpected end of JSON input'
  }

  get __gleam_prelude_variant__() {
    return "UnexpectedEndOfInput"
  }
}

export class UnexpectedByte extends CustomType {
  static regex = /Unexpected token (.) in JSON at position (\d+)/

  static isInstance(err) {
    return this.regex.test(err)
  }

  constructor(err) {
    super()
    const match = UnexpectedByte.regex.exec(err.message)
    this.byte = "0x" + match[1].charCodeAt(0).toString(16).toUpperCase()
    this.position = Number(match[2])
  }

  get __gleam_prelude_variant__() {
    return `UnexpectedByte`;
  }
}

export class UnexpectedSequence extends CustomType {
  static isInstance(err) {
    return false
  }

  get __gleam_prelude_variant__() {
    return "UnexpectedSequence";
  }
}

export class UnexpectedFormat extends CustomType {
  static isInstance(err) {
    return false
  }

  constructor(decodeErrs) {
    super()
    this[0] = decodeErrs
  }

  get __gleam_prelude_variant__() {
    return "UnexpectedFormat";
  }
}

export function json_to_string(json) {
  return JSON.stringify(json)
}

export function object_from(entries) {
  return Object.fromEntries(entries)
}

export function array(list) {
  return list.toArray()
}

export function do_null() {
  return null
}

export function identity(x) {
  return x
}

export function decode(bit_string) {
  const stringResult = bit_string_to_string(bit_string)
  if (!stringResult.isOk()) return stringResult
  try {
    const result = JSON.parse(stringResult[0])
    return new Ok(result)
  } catch (err) {
    return getJsonDecodeError(err)
  }
}

function getJsonDecodeError(stdErr) {
  const ErrClass = [
    UnexpectedByte,
    UnexpectedEndOfInput,
    UnexpectedFormat,
    UnexpectedSequence,
  ].find((klass) => klass.isInstance(stdErr))

  if (ErrClass) return new Error(new ErrClass(stdErr))
  else return new Error()
}
