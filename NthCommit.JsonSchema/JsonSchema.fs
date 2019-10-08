namespace NthCommit.JsonSchema

open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
type JsonPrimitive =
    | Unknown   = 0
    | Null      = 1
    | Boolean   = 2
    | Number    = 3
    | String    = 4
    | Array     = 5
    | Object    = 6

type JsonReference = JsonReference of string

[<RequireQualifiedAccess>]
type JsonStringSchema =
    | Enum          of string list
    | Const         of string
    | Unvalidated

type JsonObjectSchema = {
    Properties              : JsonPropertySchema list
    PatternProperties       : (Regex * JsonPropertySchema) list
    Required                : string list
    AdditionalProperties    : bool }

and JsonPropertySchema =
    | Standard  of (string * JsonSchema)
    | Reference of JsonReference

    member this.Name =
        match this with
        | Standard (name, _)    -> name
        | Reference _           -> "$ref"

    member this.Value =
        match this with
        | Standard (_, schema)  -> schema
        | Reference _           -> raise (Exception "TODO: Look up schema from path")

and JsonArraySchema = {
    Items : JsonSchema }

and JsonSchema =
    | Null
    | Number
    | Boolean
    | String        of JsonStringSchema
    | Object        of JsonObjectSchema
    | Array         of JsonArraySchema
    | Unvalidated

    member this.Primitive =
        match this with
        | String _  -> JsonPrimitive.String
        | Object _  -> JsonPrimitive.Object
        | _         -> JsonPrimitive.Unknown

[<AutoOpen>]
module META_SCHEMA =
    let META_SCHEMA = JsonSchema.Object {
        Properties = [
            JsonPropertySchema.Standard (
                "type",
                JsonSchema.String <| JsonStringSchema.Enum ["null"; "boolean"; "number"; "string"; "object"; "array"])
            JsonPropertySchema.Standard (
                "properties",
                JsonSchema.Object {
                    Properties = []
                    PatternProperties = [
                        (Regex ".*", JsonPropertySchema.Reference <| JsonReference "#") ]
                    Required = []
                    AdditionalProperties = true })]
        PatternProperties = []
        Required = []
        AdditionalProperties = true }