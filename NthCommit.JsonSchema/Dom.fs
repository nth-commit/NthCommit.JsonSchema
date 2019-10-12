namespace NthCommit.JsonSchema

open System.Text.RegularExpressions
open System

module Dom =

    type JsonReference = JsonReference of string

    [<RequireQualifiedAccess>]
    type JsonSchemaString =
        | Enum          of Set<string>
        | Const         of string
        | Unvalidated

    type JsonSchemaObject = {
        Properties              : JsonSchemaObjectProperty list
        PatternProperties       : (Regex * JsonSchemaObjectProperty) list
        Required                : Set<string>
        AdditionalProperties    : bool }

    and JsonSchemaObjectProperty =
        | Inline    of (string * JsonSchemaElement)
        | Reference of JsonReference

        member this.Name =
            match this with
            | Inline (name, _)      -> name
            | Reference _           -> "$ref"

    and JsonSchemaArray = {
        Items : JsonSchemaElement }

    and JsonSchemaElement =
        | Null
        | Number
        | Boolean
        | String        of JsonSchemaString
        | Object        of JsonSchemaObject
        | Array         of JsonSchemaArray
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