import { Ok, Error } from './gleam.mjs'
import { bit_string_to_string } from '../../gleam_stdlib/dist/gleam_stdlib.mjs'
import { UnexpectedByte, UnexpectedEndOfInput } from './gleam/json.mjs'

export function json_to_string(json) {
  return JSON.stringify(json)
}

export function object(entries) {
  return Object.fromEntries(entries)
}

export function identity(x) {
  return x
}

export function array(list) {
  return list.toArray()
}

export function do_null() {
  return null
}

export function decode(bit_string) {
  const stringResult = bit_string_to_string(bit_string)
  if (!stringResult.isOk()) return stringResult
  try {
    const result = JSON.parse(stringResult[0])
    return new Ok(result)
  } catch (err) {
    return new Error(getJsonDecodeError(err))
  }
}

export function decode_string_to_dynamic(string) {
  try {
    const result = JSON.parse(string)
    return new Ok(result)
  } catch (err) {
    return new Error(getJsonDecodeError(err))
  }
}

function getJsonDecodeError(stdErr) {
  if (isUnexpectedByte(stdErr)) return new toUnexpectedByteError(stdErr)
  if (isUnexpectedEndOfInput(stdErr)) return new UnexpectedEndOfInput()
  return undefined
}

function isUnexpectedEndOfInput(err) {
  return err.message === 'Unexpected end of JSON input'
}

const unexpectedByteRegex = /Unexpected token (.) in JSON at position (\d+)/

function isUnexpectedByte(err) {
  return unexpectedByteRegex.test(err)
}

function toUnexpectedByteError(err) {
  const match = unexpectedByteRegex.exec(err.message)
  const byte = "0x" + match[1].charCodeAt(0).toString(16).toUpperCase()
  const position = Number(match[2])
  return new UnexpectedByte(byte, position)
}
