-module(gleam_json_ffi).

-export([
    decode/1, json_to_iodata/1, json_to_string/1, int/1, float/1, string/1,
    bool/1, null/0, array/1, object/1
]).

decode(Json) ->
    thoas:decode(Json).

json_to_iodata(Json) ->
    Json.

json_to_string(Json) when is_binary(Json) ->
    Json;
json_to_string(Json) when is_list(Json) ->
    list_to_binary(Json).

null() -> thoas_encode:null().
int(X) -> thoas_encode:integer(X).
bool(X) -> thoas_encode:boolean(X).
float(X) -> thoas_encode:float(X).
string(X) -> thoas_encode:string(X).
object(X) -> thoas_encode:non_recursive_object(X).
array(X) -> thoas_encode:non_recursive_array(X).
