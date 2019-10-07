namespace NthCommit.JsonSchema

open System.Text.RegularExpressions

type JsonObjectSchema = {
    Properties : (string * JsonSchema) list
    Required : string list
    AdditionalProperties : bool }

and JsonArraySchema = {
    Items : JsonSchema }

and JsonSchema =
    | Null
    | String
    | Number
    | Boolean
    | Object of JsonObjectSchema
    | Array of JsonSchema
    | Unvalidated
