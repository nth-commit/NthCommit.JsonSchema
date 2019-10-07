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

[<RequireQualifiedAccess>]
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

    member this.Value =
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
    Path : string
    ExpectedTypes : Set<JsonPrimitive>
    ActualType : JsonPrimitive }

type SchemaValueError = {
    Path : string
    Value : string }

[<RequireQualifiedAccess>]
type SchemaError =
    | Type of SchemaTypeError
    | Value of SchemaValueError

[<RequireQualifiedAccess>]
type ParserError =
    | Json
    | Schema of SchemaError

[<AutoOpen>]
module Parser =

    let metaSchema : JsonSchema =
        Object {
            Properties = [
                Standard ("type", String <| JsonStringSchema.Enum ["null"; "boolean"; "number"; "string"; "object"; "array"])
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

    let private stringMatchesSchema (schema : JsonStringSchema) (instance : string) (path : string) : Result<JsonSchema, SchemaError> =
        match schema with
        | JsonStringSchema.Unvalidated -> Ok Unvalidated
        | JsonStringSchema.Enum values ->
            if values |> List.contains instance
            then Ok <| String JsonStringSchema.Unvalidated
            else Error <| SchemaError.Value {
                Path = path
                Value = instance }
        | JsonStringSchema.Const _ -> raise (Exception ("Unhandled: JsonStringSchema.Const"))

    let rec private propertyMatchesSchema (schema : JsonPropertySchema) (instance : JProperty) : Result<JsonSchema, SchemaError> =
        let schemaPrimitive = schema.Value.Primitive
        let instancePrimitive = instance.Value.Type |> JTokenType.toPrimitive
        if schemaPrimitive = instancePrimitive
        then
            match schema with
            | Standard (_, valueSchema) -> tokenMatchesSchema valueSchema instance.Value
            | _ -> raise (Exception ())
        else Error <| SchemaError.Type {
            Path = instance.Path
            ExpectedTypes = Set([schemaPrimitive])
            ActualType = instancePrimitive }

    and private objectMatchesSchema (schema : JsonObjectSchema) (instance : JProperty list) : Result<JsonSchema, SchemaError> =
        match instance |> List.tryHead with
        | Some instanceProperty ->
            let schemaProperty =
                schema.Properties
                |> List.find (fun property -> property.Name = instanceProperty.Name)
            propertyMatchesSchema schemaProperty instanceProperty
        | None -> Ok Unvalidated

    and private tokenMatchesSchema (schema : JsonSchema) (jToken : JToken) : Result<JsonSchema, SchemaError> =
        match (schema, matchJToken jToken) with
        | JsonSchema.Object objectSchema, MatchedJObject objectInstance ->
            objectMatchesSchema objectSchema objectInstance
        | JsonSchema.String stringSchema, MatchedJValueAsString stringInstance ->
            stringMatchesSchema stringSchema stringInstance jToken.Path
        | _, _ ->
            let expectedTypes = Set([JsonPrimitive.Object])
            let actualType = jToken.Type |> JTokenType.toPrimitive
            SchemaError.Type {
                Path = jToken.Path
                ExpectedTypes = expectedTypes
                ActualType = actualType } |> Error

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