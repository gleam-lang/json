import { Ok, Error } from './gleam.mjs'
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

export function decode(string) {
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
  return new UnexpectedByte('', 0)
}

function isUnexpectedEndOfInput(err) {
  return err.message.includes('Unexpected end') || err.message.includes('Unexpected EOF')
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
