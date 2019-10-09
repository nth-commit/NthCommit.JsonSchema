namespace NthCommit.JsonSchema

open System.Text.RegularExpressions
open System

module Dom =

    type JsonReference = JsonReference of string

    [<RequireQualifiedAccess>]
    type JsonString =
        | Enum          of string list
        | Const         of string
        | Unvalidated

    type JsonObject = {
        Properties              : JsonObjectProperty list
        PatternProperties       : (Regex * JsonObjectProperty) list
        Required                : string list
        AdditionalProperties    : bool }

    and JsonObjectProperty =
        | Inline    of (string * JsonDocument)
        | Reference of JsonReference

        member this.Name =
            match this with
            | Inline (name, _)      -> name
            | Reference _           -> "$ref"

    and JsonArray = {
        Items : JsonDocument }

    and JsonDocument =
        | Null
        | Number
        | Boolean
        | String        of JsonString
        | Object        of JsonObject
        | Array         of JsonArray
        | Unvalidated

        member this.Primitive =
            match this with
            | Null      -> JsonPrimitive.Null
            | Boolean   -> JsonPrimitive.Boolean
            | Number    -> JsonPrimitive.Number
            | String _  -> JsonPrimitive.String
            | Array _   -> JsonPrimitive.Array
            | Object _  -> JsonPrimitive.Object
            | _         -> raise (Exception ("Unhandled JsonDocument"))