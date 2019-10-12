namespace NthCommit.JsonSchema

open System.Text.RegularExpressions
open System

module Dom =

    type JsonReference = JsonReference of string

    [<RequireQualifiedAccess>]
    type JsonStringSchema =
        | Enum of Set<string>
        | Const of string
        | Unvalidated

    type JsonObjectSchema = {
        Properties : JsonPropertySchema list
        PatternProperties : (Regex * JsonPropertySchema) list
        Required : Set<string>
        AdditionalProperties : bool }

    and JsonPropertySchema =
        | Inline    of (string * JsonElementSchema)
        | Reference of JsonReference

        member this.Name =
            match this with
            | Inline (name, _)      -> name
            | Reference _           -> "$ref"

    and JsonArraySchema = {
        Items : JsonElementSchema }

    and JsonElementSchema =
        | Null
        | Number
        | Boolean
        | String of JsonStringSchema
        | Object of JsonObjectSchema
        | Array of JsonArraySchema
        | Unvalidated

        member this.Primitive =
            match this with
            | Null -> JsonPrimitive.Null
            | Boolean -> JsonPrimitive.Boolean
            | Number -> JsonPrimitive.Number
            | String _ -> JsonPrimitive.String
            | Array _ -> JsonPrimitive.Array
            | Object _ -> JsonPrimitive.Object
            | _ -> raise (Exception ("Unhandled JsonDocument"))