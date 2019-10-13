namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper
open NthCommit.JsonSchema.Validator

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

        module Option =

            let bindOr (f : unit -> 'a option) x =
                match x with
                | Some x' -> Some x'
                | None ->
                    match f () with
                    | Some x' -> Some x'
                    | None -> None

        let tryParseEnum (propertiesByName : Map<string, JProperty>) =
            propertiesByName |> Map.tryFind "enum"
            |> Option.map (fun enumProperty ->
                (enumProperty.Value :?> JArray)
                |> Seq.map (fun enumValueToken -> enumValueToken.Value<string>())
                |> Set
                |> JsonStringSchema.Enum) : Option<JsonStringSchema>

        let tryParseConst (propertiesByName : Map<string, JProperty>) =
            propertiesByName |> Map.tryFind "const"
            |> Option.map (fun constProperty ->
                constProperty.Value.Value<string>()
                |> JsonStringSchema.Const) : Option<JsonStringSchema>

        let parseString (propertiesByName : Map<string, JProperty>) =
            tryParseEnum propertiesByName
            |> Option.bindOr (fun _ -> tryParseConst propertiesByName)
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

        let private parseRequired (propertiesByName : Map<string, JProperty>) =
            match propertiesByName |> Map.tryFind "required" with
            | Some requiredProperty ->
                (requiredProperty.Value :?> JArray).AsJEnumerable()
                |> Seq.map (fun t -> t.Value<string>())
            | None -> Seq.empty
            |> Set

        let private parseAdditionalProperties (propertiesByName : Map<string, JProperty>) =
            propertiesByName
            |> Map.tryFind "additionalProperties"
            |> Option.map (fun additionalProperties -> additionalProperties.Value.Value<bool>())
            |> Option.defaultValue true

        let parseObject parseSchemaToken (propertiesByName : Map<string, JProperty>) =
            JsonElementSchema.Object <| {
                Properties = parseSchemaProperties parseSchemaToken propertiesByName
                PatternProperties = parseSchemaPatternProperties parseSchemaToken propertiesByName
                Required = parseRequired propertiesByName
                AdditionalProperties = parseAdditionalProperties propertiesByName }

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
        |> Option.map (fun p -> p.Value.Value<string>())

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
        | Some jtype -> parseSchemaOfType parseSchemaToken propertiesByName jtype
        | None -> JsonElementSchema.Unvalidated

    let rec private parseSchemaToken schemaToken : JsonElementSchema =
        match matchJToken schemaToken with
        | MatchedJObject properties ->
            let propertiesByName = keyPropertiesByName properties
            parseSchema parseSchemaToken propertiesByName
        | _ -> raiseUnhandledToken schemaToken

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
