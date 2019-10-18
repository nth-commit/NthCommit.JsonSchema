namespace NthCommit.JsonSchema.Parsing

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper
open NthCommit.JsonSchema.Validation

type UnhandledJTokenException(token : JToken) =
    inherit Exception(sprintf "Unexpected token: %s" token.Path)

type UnhandledValueException<'a>(value : 'a) =
    inherit Exception(sprintf "Unhandled value: %s" (value.ToString()))

[<AutoOpen>]
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

    let private keyPropertiesByName (objectInstance : JsonObjectInstance) : Map<string, JsonPropertyInstance> =
        objectInstance.Properties
        |> List.toMap (fun p -> p.Name)

    let private tryGetTypeProperty (propertiesByName : Map<string, JsonPropertyInstance>) =
        propertiesByName
        |> Map.tryFind "type"
        |> Option.map (fun p -> p.Value.Value<string>())

    let private parseSchemaOfType
        (parseSchemaToken : JToken -> JsonElementSchema)
        (propertiesByName : Map<string, JsonPropertyInstance>) = function
            | "null" -> JsonElementSchema.Null
            | "boolean" -> JsonElementSchema.Boolean
            | "number" -> JsonElementSchema.Number
            | "string" -> Strings.parse propertiesByName
            | "array" -> Arrays.parse parseSchemaToken propertiesByName
            | "object" -> Objects.parse parseSchemaToken propertiesByName
            | x -> raiseUnhandledValue x

    let private parseSchema parseSchemaToken (propertiesByName : Map<string, JsonPropertyInstance>) =
        match tryGetTypeProperty propertiesByName with
        | Some jtype -> parseSchemaOfType parseSchemaToken propertiesByName jtype
        | None -> JsonElementSchema.Unvalidated

    let rec private parseSchemaToken schemaToken : JsonElementSchema =
        match matchJToken schemaToken with
        | JsonElementInstance.Object objectInstance ->
            let propertiesByName = keyPropertiesByName objectInstance
            parseSchema parseSchemaToken propertiesByName
        | _ -> raiseUnhandledToken schemaToken

    let private META_SCHEMA = JsonElementSchema.Object {
        Properties = [
            JsonPropertySchema.Inline (
                "type",
                JsonElementSchema.String <|
                    JsonStringSchema.Enum (Set(["null"; "boolean"; "number"; "string"; "object"; "array"])))
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

    let parse (schema : string) : Result<JsonElementSchema, List<SchemaError>> =
        match validate META_SCHEMA schema with
        | Ok schemaToken -> parseSchemaToken schemaToken |> Ok
        | Error errors -> errors |> Error
