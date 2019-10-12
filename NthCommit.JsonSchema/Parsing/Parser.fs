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
            | JsonPrimitive.Null    -> JsonElementSchema.Null
            | JsonPrimitive.Boolean -> JsonElementSchema.Boolean
            | JsonPrimitive.Number  -> JsonElementSchema.Number
            | JsonPrimitive.String  -> JsonElementSchema.String    <| JsonStringSchema.Unvalidated
            | JsonPrimitive.Array   -> JsonElementSchema.Array     <| { Items = JsonElementSchema.Unvalidated }
            | JsonPrimitive.Object  -> JsonElementSchema.Object    <| {
                Properties = []
                PatternProperties = []
                Required = Set []
                AdditionalProperties = true }
        | None -> JsonElementSchema.Unvalidated

    let private parseSchema (properties : JProperty list) =
        let propertiesByName = keyPropertiesByName properties
        propertiesByName
        |> Map.tryFind "type"
        |> Option.map JProperty.stringValue
        |> Option.map parseType
        |> makeSchema

    let private parseSchemaToken schemaToken : Result<JsonElementSchema, ParserError> =
        match matchJToken schemaToken with
        | MatchedJObject properties -> parseSchema properties |> Ok
        | _                         -> raiseUnhandledToken schemaToken

    let private META_SCHEMA = JsonElementSchema.Object {
        Properties = [
            JsonPropertySchema.Inline (
                "type",
                JsonElementSchema.String <| JsonStringSchema.Enum (Set(["null"; "boolean"; "number"; "string"; "object"; "array"])))
            JsonPropertySchema.Inline (
                "properties",
                JsonElementSchema.Object {
                    Properties = []
                    PatternProperties = [
                        (Regex ".*", JsonPropertySchema.Reference <| JsonReference "#") ]
                    Required = Set []
                    AdditionalProperties = true })]
        PatternProperties = []
        Required = Set []
        AdditionalProperties = true }

    let parse (schema : string) : Result<JsonElementSchema, ParserError> =
        match validate META_SCHEMA schema with
        | Ok schemaToken -> parseSchemaToken schemaToken
        | Error errors -> errors |> List.head |> ParserError.Schema |> Error
