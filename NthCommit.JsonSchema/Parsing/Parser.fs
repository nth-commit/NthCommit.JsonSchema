namespace NthCommit.JsonSchema

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
            | JsonPrimitive.Null    -> JsonSchemaElement.Null
            | JsonPrimitive.Boolean -> JsonSchemaElement.Boolean
            | JsonPrimitive.Number  -> JsonSchemaElement.Number
            | JsonPrimitive.String  -> JsonSchemaElement.String    <| JsonSchemaString.Unvalidated
            | JsonPrimitive.Array   -> JsonSchemaElement.Array     <| { Items = JsonSchemaElement.Unvalidated }
            | JsonPrimitive.Object  -> JsonSchemaElement.Object    <| {
                Properties = []
                PatternProperties = []
                Required = []
                AdditionalProperties = true }
        | None -> JsonSchemaElement.Unvalidated

    let private parseSchema (properties : JProperty list) =
        let propertiesByName = keyPropertiesByName properties
        propertiesByName
        |> Map.tryFind "type"
        |> Option.map JProperty.stringValue
        |> Option.map parseType
        |> makeSchema

    let private parseSchemaToken schemaToken : Result<JsonSchemaElement, ParserError> =
        match matchJToken schemaToken with
        | MatchedJObject properties -> parseSchema properties |> Ok
        | _                         -> raiseUnhandledToken schemaToken

    let private META_SCHEMA = JsonSchemaElement.Object {
        Properties = [
            JsonSchemaObjectProperty.Inline (
                "type",
                JsonSchemaElement.String <| JsonSchemaString.Enum (Set(["null"; "boolean"; "number"; "string"; "object"; "array"])))
            JsonSchemaObjectProperty.Inline (
                "properties",
                JsonSchemaElement.Object {
                    Properties = []
                    PatternProperties = [
                        (Regex ".*", JsonSchemaObjectProperty.Reference <| JsonReference "#") ]
                    Required = []
                    AdditionalProperties = true })]
        PatternProperties = []
        Required = []
        AdditionalProperties = true }

    let parse (schema : string) : Result<JsonSchemaElement, ParserError> =
        match validate META_SCHEMA schema with
        | Ok schemaToken -> parseSchemaToken schemaToken
        | Error errors -> errors |> List.head |> ParserError.Schema |> Error
