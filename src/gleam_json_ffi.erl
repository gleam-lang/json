-module(gleam_json_ffi).

-export([json_to_iodata/1, json_to_string/1]).

json_to_iodata(Json) ->
    Json.

json_to_string(Json) when is_binary(Json) ->
    Json;
json_to_string(Json) when is_list(Json) ->
    list_to_binary(Json).
