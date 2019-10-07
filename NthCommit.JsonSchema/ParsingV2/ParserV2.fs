namespace NthCommit.JsonSchema.V2

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.JsonHelper
open System.Text.RegularExpressions
open Newtonsoft.Json

[<RequireQualifiedAccess>]
type JsonPrimitive =
    | Unknown = 0
    | Object = 1
    | String = 2
    | Boolean = 3

type JsonReference = JsonReference of string

type JsonStringSchema =
    | Enum of string list
    | Const of string
    | Unvalidated

type JsonObjectSchema = {
    Properties : JsonPropertySchema list
    PatternProperties : (Regex * JsonSchema) list
    Required : string list
    AdditionalProperties : bool }

and JsonPropertySchema =
    | Standard of (string * JsonSchema)
    | Reference of JsonReference

    member this.Name =
        match this with
        | Standard (name, _) -> name
        | Reference _ -> "$ref"

    member this.ValueSchema =
        match this with
        | Standard (_, schema) -> schema
        | Reference _ -> raise (Exception "TODO: Look up schema from path")

and JsonArraySchema = {
    Items : JsonSchema }

and JsonSchema =
    | Null
    | Number
    | Boolean
    | String of JsonStringSchema
    | Object of JsonObjectSchema
    | Array of JsonArraySchema
    | Unvalidated

    member this.Primitive =
        match this with
        | String _ -> JsonPrimitive.String
        | Object _ -> JsonPrimitive.Object
        | _ -> JsonPrimitive.Unknown

module JTokenType =

    let toPrimitive = function
        | JTokenType.String -> JsonPrimitive.String
        | JTokenType.Boolean -> JsonPrimitive.Boolean
        | _ -> JsonPrimitive.Unknown


type SchemaTypeError = {
    ExpectedTypes : Set<JsonPrimitive>
    ActualType : JsonPrimitive }

[<RequireQualifiedAccess>]
type SchemaError =
    | Type of SchemaTypeError
    | Value of string

[<RequireQualifiedAccess>]
type ParserError =
    | Json
    | Schema of SchemaError

[<AutoOpen>]
module Parser =

    let metaSchema : JsonSchema =
        Object {
            Properties = [
                Standard ("type", String <| Enum ["null"; "boolean"; "number"; "string"; "object"; "array"])
                Standard ("properties", Object {
                    Properties = []
                    PatternProperties = [
                        (Regex ".*", Object {
                            Properties = [ 
                                Reference <| JsonReference "#" ]
                            PatternProperties = []
                            Required = []
                            AdditionalProperties = true })]
                    Required = []
                    AdditionalProperties = true })]
            PatternProperties = []
            Required = []
            AdditionalProperties = true }

    type ParserResult = Result<JsonSchema, ParserError>

    let private objectMatchesSchema (schema : JsonObjectSchema) (properties : JProperty list) : Result<JsonSchema, SchemaError> =
        match properties |> List.tryHead with
        | Some actualProperty ->
            let expectedProperty =
                schema.Properties
                |> List.find (fun property -> property.Name = actualProperty.Name)
            let actualPrimitive = actualProperty.Value.Type |> JTokenType.toPrimitive
            if expectedProperty.ValueSchema.Primitive = actualPrimitive
            then Error (actualProperty.Value.ToString() |> SchemaError.Value)
            else Error (SchemaError.Type { ExpectedTypes = Set([expectedProperty.ValueSchema.Primitive]); ActualType = actualPrimitive })
        | None -> Ok Unvalidated

    let private tokenMatchesSchema (schema : JsonSchema) (jToken : JToken) : Result<JsonSchema, SchemaError> =
        match (schema, matchJToken jToken) with
        | JsonSchema.Object jsonObjectSchema, MatchedJObject jProperties ->
            objectMatchesSchema jsonObjectSchema jProperties
        | _, _ ->
            let expectedTypes = Set([JsonPrimitive.Object])
            let actualType = jToken.Type |> JTokenType.toPrimitive
            SchemaError.Type { ExpectedTypes = expectedTypes; ActualType = actualType } |> Error

    let private schemaMatchesMetaSchema (jToken : JToken) : ParserResult =
        match tokenMatchesSchema metaSchema jToken with
        | Ok schema -> schema |> Ok
        | Error schema -> schema |> ParserError.Schema |> Error

    let private deserialize schema =
        tryDeserialize schema
        |> Result.mapError (fun _ -> ParserError.Json)

    let parse (schema : string) : ParserResult =
        deserialize schema
        |> Result.bind schemaMatchesMetaSchema