namespace NthCommit.JsonSchema.Parsing

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper

module Objects =

    let private parseSchemaProperty
        (parseSchemaToken : JToken -> JsonElementSchema)
        (property : JProperty) : JsonPropertySchema =
            match property.Name with
            | "$ref" -> Reference(JsonReference (property.Value.Value<string>()))
            | propertyName -> Inline(propertyName, parseSchemaToken property.Value)

    let private parseSubSchema
        (parseSchemaToken : JToken -> JsonElementSchema)
        (subSchemaToken : JToken) =
            (subSchemaToken :?> JObject).Properties()
            |> Seq.map (parseSchemaProperty parseSchemaToken)
            |> Seq.toList

    let private parseSchemaProperties
        (parseSchemaToken : JToken -> JsonElementSchema)
        (propertiesByName : Map<string, JsonPropertyInstance>) =
            match propertiesByName |> Map.tryFind "properties" with
            | Some propertiesProperty -> parseSubSchema parseSchemaToken propertiesProperty.Value
            | None -> []

    let private parseSchemaPatternProperties
        (parseSchemaToken : JToken -> JsonElementSchema)
        (propertiesByName : Map<string, JsonPropertyInstance>) =
            match propertiesByName |> Map.tryFind "patternProperties" with
            | Some patternPropertiesProperty ->
                (patternPropertiesProperty.Value :?> JObject).Properties()
                |> Seq.map (fun p -> (RegularExpression p.Name, parseSubSchema parseSchemaToken p.Value))
                |> Seq.toList
            | None -> []

    let private parseRequired (propertiesByName : Map<string, JsonPropertyInstance>) =
        match propertiesByName |> Map.tryFind "required" with
        | Some requiredProperty ->
            (requiredProperty.Value :?> JArray).AsJEnumerable()
            |> Seq.map (fun t -> t.Value<string>())
        | None -> Seq.empty
        |> Set

    let private parseAdditionalProperties (propertiesByName : Map<string, JsonPropertyInstance>) =
        propertiesByName
        |> Map.tryFind "additionalProperties"
        |> Option.map (fun additionalProperties -> additionalProperties.Value.Value<bool>())
        |> Option.defaultValue true

    let parse
        (parseSchemaToken : JToken -> JsonElementSchema)
        (propertiesByName : Map<string, JsonPropertyInstance>) =
            JsonElementSchema.Object <| {
                Properties = parseSchemaProperties parseSchemaToken propertiesByName
                PatternProperties = parseSchemaPatternProperties parseSchemaToken propertiesByName
                Required = parseRequired propertiesByName
                AdditionalProperties = parseAdditionalProperties propertiesByName }