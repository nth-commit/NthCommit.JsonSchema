﻿namespace NthCommit.JsonSchema

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper
open Validator
open System
open System.Text.RegularExpressions

module private JProperty =
    let stringValue (property : JProperty) = property.Value.Value<string>()

[<RequireQualifiedAccess>]
type ParserError =
    | Json
    | Schema of SchemaError
    | Semantic

type UnhandledJTokenException(token : JToken) =
    inherit Exception(sprintf "Unexpected token: %s" token.Path)

type UnhandledValueException<'a>(value : 'a) =
    inherit Exception(sprintf "Unhandled value: %s" (value.ToString()))

module Parser =

    module private List =

        let toMap keyProjection list =
            list
            |> List.map (fun x -> (keyProjection x, x))
            |> Map

    let private raiseUnhandledToken token =
        raise (UnhandledJTokenException token)

    let private raiseUnhandledValue value =
        raise (UnhandledValueException value)

    let private keyPropertiesByName (properties : JProperty list) : Map<string, JProperty> =
        properties
        |> List.toMap (fun p -> p.Name)

    let private parseType = function
        | "null"    -> JsonPrimitive.Null
        | "boolean" -> JsonPrimitive.Boolean
        | "number"  -> JsonPrimitive.Number
        | "string"  -> JsonPrimitive.String
        | "array"   -> JsonPrimitive.Array
        | "object"  -> JsonPrimitive.Object
        | x         -> raiseUnhandledValue x

    let private makeSchema jsonPrimitiveOpt =
        match jsonPrimitiveOpt with
        | Some jsonPrimitive ->
            match jsonPrimitive with
            | JsonPrimitive.Null    -> JsonDocument.Null
            | JsonPrimitive.Boolean -> JsonDocument.Boolean
            | JsonPrimitive.Number  -> JsonDocument.Number
            | JsonPrimitive.String  -> JsonDocument.String    <| JsonString.Unvalidated
            | JsonPrimitive.Array   -> JsonDocument.Array     <| { Items = JsonDocument.Unvalidated }
            | JsonPrimitive.Object  -> JsonDocument.Object    <| {
                Properties = []
                PatternProperties = []
                Required = []
                AdditionalProperties = true }
            | _                     -> raise (Exception ("TODO"))
        | None -> JsonDocument.Unvalidated

    let private parseSchema (properties : JProperty list) =
        let propertiesByName = keyPropertiesByName properties
        propertiesByName
        |> Map.tryFind "type"
        |> Option.map JProperty.stringValue
        |> Option.map parseType
        |> makeSchema

    let private parseSchemaToken schemaToken : Result<JsonDocument, ParserError> =
        match matchJToken schemaToken with
        | MatchedJObject properties -> parseSchema properties |> Ok
        | _                         -> raiseUnhandledToken schemaToken

    let private deserialize schema =
        tryDeserialize schema
        |> Result.mapError (fun _ -> ParserError.Json)

    let private META_SCHEMA = JsonDocument.Object {
        Properties = [
            JsonObjectProperty.Inline (
                "type",
                JsonDocument.String <| JsonString.Enum ["null"; "boolean"; "number"; "string"; "object"; "array"])
            JsonObjectProperty.Inline (
                "properties",
                JsonDocument.Object {
                    Properties = []
                    PatternProperties = [
                        (Regex ".*", JsonObjectProperty.Reference <| JsonReference "#") ]
                    Required = []
                    AdditionalProperties = true })]
        PatternProperties = []
        Required = []
        AdditionalProperties = true }

    let parse (schema : string) : Result<JsonDocument, ParserError> =
        deserialize schema
        |> Result.bind (fun schemaToken ->
            match validate META_SCHEMA schemaToken with
            | []        -> parseSchemaToken schemaToken
            | errors    -> errors |> List.head |> ParserError.Schema |> Error)
