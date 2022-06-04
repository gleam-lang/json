import { Ok, Error } from './gleam.mjs'
import { UnexpectedByte, UnexpectedEndOfInput } from './gleam/json.mjs'

const Runtime = {
  chromium: 'chromium',
  spidermonkey: 'spidermonkey',
  jscore: 'jscore',
}

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
    return new Error(getJsonDecodeError(err, string))
  }
}

function getJsonDecodeError(stdErr, json) {
  if (isUnexpectedEndOfInput(stdErr)) return new UnexpectedEndOfInput()
  const unexpectedByteRuntime = getUnexpectedByteRuntime(stdErr)
  if (unexpectedByteRuntime) return toUnexpectedByteError(unexpectedByteRuntime, stdErr, json)
  return new UnexpectedByte('', 0)
}

/**
 * Matches unexpected end of input messages in:
 * - Chromium (edge, chrome, node)
 * - Spidermonkey (firefox)
 * - JavascriptCore (safari)
 * 
 * Note that Spidermonkey and JavascriptCore will both incorrectly report some
 * UnexpectedByte errors as UnexpectedEndOfInput errors. For example:
 * 
 * @example
 * // in JavascriptCore
 * JSON.parse('{"a"]: "b"})
 * // => JSON Parse error: Expected ':' before value
 * 
 * JSON.parse('{"a"')
 * // => JSON Parse error: Expected ':' before value
 * 
 * // in Chromium (correct)
 * JSON.parse('{"a"]: "b"})
 * // => Unexpected token ] in JSON at position 4
 * 
 * JSON.parse('{"a"')
 * // => Unexpected end of JSON input
 */
const unexpectedEndOfInputRegex = /((unexpected (end|eof))|(end of data)|(unterminated string)|(json( parse error|\.parse)\: expected '(\:|\}|\])'))/i

function isUnexpectedEndOfInput(err) {
  return unexpectedEndOfInputRegex.test(err.message)
}


/**
 * Matches unexpected byte messages in:
 * - Chromium (edge, chrome, node)
 * 
 * Matches the character and its position.
 */
const chromiumUnexpectedByteRegex = /unexpected token (.) in JSON at position (\d+)/

/**
 * Matches unexpected byte messages in:
 * - JavascriptCore (safari)
 * 
 * JavascriptCore only reports what the character is and not its position.
 */
const jsCoreUnexpectedByteRegex = /unexpected identifier "(.)"/i

/**
 * Matches unexpected byte messages in:
 * - Spidermonkey (firefox)
 * 
 * Matches the position in a 2d grid only and not the character.
 */
const spidermonkeyUnexpectedByteRegex = /((unexpected character|expected double-quoted property name) at line (\d+) column (\d+))/i

function getUnexpectedByteRuntime(err) {
  if (chromiumUnexpectedByteRegex.test(err.message)) return Runtime.chromium
  if (jsCoreUnexpectedByteRegex.test(err.message)) return Runtime.jscore
  if (spidermonkeyUnexpectedByteRegex.test(err.message)) return Runtime.spidermonkey
  return null
}

/**
 * Converts a SyntaxError to an UnexpectedByte error based on the JS runtime.
 * 
 * For Chromium, the unexpected byte and position are reported by the runtime.
 * 
 * For JavascriptCore, only the unexpected byte is reported by the runtime, so
 * there is no way to know which position that character is in unless we then
 * parse the string again ourselves. So instead, the position is reported as 0.
 * 
 * For Spidermonkey, the position is reported by the runtime as a line and column number 
 * and the unexpected byte is found using those coordinates.
 * 
 * @param {'chromium' | 'spidermonkey' | 'jscore'} runtime 
 * @param {SyntaxError} err 
 * @param {string} json 
 * @returns {UnexpectedByte}
 */
function toUnexpectedByteError(runtime, err, json) {
  switch (runtime) {
    case Runtime.chromium:
      return toChromiumUnexpectedByteError(err)
    case Runtime.spidermonkey:
      return toSpidermonkeyUnexpectedByteError(err, json)
    case Runtime.jscore:
      return toJsCoreUnexpectedByteError(err)
  }
}

function toChromiumUnexpectedByteError(err) {
  const match = chromiumUnexpectedByteRegex.exec(err.message)
  const byte = toHex(match[1])
  const position = Number(match[2])
  return new UnexpectedByte(byte, position)
}

function toSpidermonkeyUnexpectedByteError(err, json) {
  const match = spidermonkeyUnexpectedByteRegex.exec(err.message)
  const line = Number(match[1])
  const column = Number(match[2])
  const position = getPositionFromMultiline(line, column, json)
  const byte = toHex(err.message[position])
  return new UnexpectedByte(byte, position)
}

function toJsCoreUnexpectedByteError(err) {
  const match = jsCoreUnexpectedByteRegex.exec(err.message)
  const byte = toHex(match[1])
  return new UnexpectedByte(byte, 0)
}

function toHex(char) {
  return "0x" + char.charCodeAt(0).toString(16).toUpperCase()
}

/**
 * Gets the position of a character in a flattened (i.e. single line) string
 * from a line and column number. Note that the position is 0-indexed and 
 * the line and column numbers are 1-indexed.
 * 
 * @param {number} line 
 * @param {number} column 
 * @param {string} string 
 */
function getPositionFromMultiline(line, column, string) {
  if (line === 1) return column - 1

  let currentLn = 1
  let position = 0
  string.split('').find((char, idx) => {
    if (char === '\n') currentLn += 1
    if (currentLn === line) {
      position = idx + column
      return true
    }
    return false
  })

  return position
}