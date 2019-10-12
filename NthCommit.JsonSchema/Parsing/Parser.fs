namespace NthCommit.JsonSchema

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper
open Validator
open System
open System.Text.RegularExpressions
open Newtonsoft.Json

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

    let private raiseUnhandledToken token =
        raise (UnhandledJTokenException token)

    let private raiseUnhandledValue value =
        raise (UnhandledValueException value)

    module Strings =

        let tryParseEnum (propertiesByName : Map<string, JProperty>) : Option<JsonStringSchema> =
            propertiesByName |> Map.tryFind "enum"
            |> Option.map (fun enumProperty ->
                (enumProperty.Value :?> JArray)
                |> Seq.map (fun enumValueToken -> enumValueToken.Value<string>())
                |> Set
                |> JsonStringSchema.Enum)

        let parseString (propertiesByName : Map<string, JProperty>) =
            tryParseEnum propertiesByName
            |> Option.defaultValue JsonStringSchema.Unvalidated
            |> JsonElementSchema.String

    module Objects =

        let private parseSchemaProperty parseSchemaToken (property : JProperty) : JsonPropertySchema =
            match property.Name with
            | "$ref" -> Reference(JsonReference (property.Value.Value<string>()))
            | propertyName -> Inline(propertyName, parseSchemaToken property.Value)

        let private parseSubSchema parseSchemaToken (subSchemaToken : JToken) =
            (subSchemaToken :?> JObject).Properties()
            |> Seq.map (parseSchemaProperty parseSchemaToken)
            |> Seq.toList

        let private parseSchemaProperties parseSchemaToken (propertiesByName : Map<string, JProperty>) =
            match propertiesByName |> Map.tryFind "properties" with
            | Some propertiesProperty -> parseSubSchema parseSchemaToken propertiesProperty.Value
            | None -> []

        let private parseSchemaPatternProperties parseSchemaToken (propertiesByName : Map<string, JProperty>) =
            match propertiesByName |> Map.tryFind "patternProperties" with
            | Some patternPropertiesProperty ->
                (patternPropertiesProperty.Value :?> JObject).Properties()
                |> Seq.map (fun p -> (RegularExpression p.Name, parseSubSchema parseSchemaToken p.Value))
                |> Seq.toList
            | None -> []

        let parseObject parseSchemaToken (propertiesByName : Map<string, JProperty>) =
            JsonElementSchema.Object <| {
                Properties = parseSchemaProperties parseSchemaToken propertiesByName
                PatternProperties = parseSchemaPatternProperties parseSchemaToken propertiesByName
                Required = Set []
                AdditionalProperties = true }

    module private List =

        let toMap keyProjection list =
            list
            |> List.map (fun x -> (keyProjection x, x))
            |> Map

    let private keyPropertiesByName (properties : JProperty list) : Map<string, JProperty> =
        properties
        |> List.toMap (fun p -> p.Name)

    let private tryGetTypeProperty (propertiesByName : Map<string, JProperty>) =
        propertiesByName
        |> Map.tryFind "type"
        |> Option.map JProperty.stringValue

    let private parseSchemaOfType parseSchemaToken (propertiesByName : Map<string, JProperty>) = function
        | "null" -> JsonElementSchema.Null
        | "boolean" -> JsonElementSchema.Boolean
        | "number" -> JsonElementSchema.Number
        | "string" -> Strings.parseString propertiesByName
        | "array" -> JsonElementSchema.Array { Items = JsonElementSchema.Unvalidated }
        | "object" -> Objects.parseObject parseSchemaToken propertiesByName
        | x -> raiseUnhandledValue x

    let private parseSchema parseSchemaToken (propertiesByName : Map<string, JProperty>) =
        match tryGetTypeProperty propertiesByName with
        | Some x -> parseSchemaOfType parseSchemaToken propertiesByName x
        | None -> JsonElementSchema.Unvalidated

    let rec private parseSchemaToken schemaToken : JsonElementSchema =
        match matchJToken schemaToken with
        | MatchedJObject properties ->
            let propertiesByName = keyPropertiesByName properties
            parseSchema parseSchemaToken propertiesByName
        | _ -> raiseUnhandledToken schemaToken

    let META_SCHEMA = JsonElementSchema.Object {
        Properties = [
            JsonPropertySchema.Inline (
                "type",
                JsonElementSchema.String <| JsonStringSchema.Enum (Set(["null"; "boolean"; "number"; "string"; "object"; "array"])))
            JsonPropertySchema.Inline (
                "properties",
                JsonElementSchema.Object {
                    Properties = []
                    PatternProperties = [
                        (RegularExpression ".*", [ JsonPropertySchema.Reference <| JsonReference "#" ])]
                    Required = Set []
                    AdditionalProperties = true })]
        PatternProperties = []
        Required = Set []
        AdditionalProperties = true }

    let parse (schema : string) : Result<JsonElementSchema, ParserError> =
        match validate META_SCHEMA schema with
        | Ok schemaToken -> parseSchemaToken schemaToken |> Ok
        | Error errors -> errors |> List.head |> ParserError.Schema |> Error
