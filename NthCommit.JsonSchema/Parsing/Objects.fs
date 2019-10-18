namespace NthCommit.JsonSchema.Parsing

open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver

module Objects =

    let private parseSchemaProperty
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (property : JsonPropertyInstance) : JsonPropertySchema =
            match property.Name with
            | "$ref" ->
                property.Value
                |> JsonDriverElement.getString
                |> (JsonReference >> Reference)
            | propertyName -> Inline(propertyName, parseSchemaToken property.Value)

    let private parseSubSchema
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (subSchemaToken : JsonDriverElement) =
            subSchemaToken
            |> JsonDriverElement.getObjectProperties
            |> List.map (parseSchemaProperty parseSchemaToken)

    let private parseSchemaProperties
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (objectInstance : JsonObjectInstance) =
            match objectInstance.TryFindProperty "properties" with
            | Some propertiesProperty -> parseSubSchema parseSchemaToken propertiesProperty.Value
            | None -> []

    let private parseSchemaPatternProperties
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (objectInstance : JsonObjectInstance) =
            match objectInstance.TryFindProperty "patternProperties" with
            | Some patternPropertiesProperty ->
                patternPropertiesProperty.Value
                |> JsonDriverElement.getObjectProperties
                |> List.map (fun p -> (RegularExpression p.Name, parseSubSchema parseSchemaToken p.Value))
            | None -> []

    let private parseRequired (objectInstance : JsonObjectInstance) =
        match objectInstance.TryFindProperty "required" with
        | Some requiredProperty ->
            requiredProperty.Value
            |> JsonDriverElement.getArray
            |> List.map JsonDriverElement.getString
        | None -> []
        |> Set

    let private parseAdditionalProperties (objectInstance : JsonObjectInstance) =
        objectInstance.TryFindProperty "additionalProperties"
        |> Option.map (fun additionalProperties ->
            additionalProperties.Value
            |> JsonDriverElement.asBoolean)
        |> Option.defaultValue true

    let parse
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (objectInstance : JsonObjectInstance) =
            JsonElementSchema.Object <| {
                Properties = parseSchemaProperties parseSchemaToken objectInstance
                PatternProperties = parseSchemaPatternProperties parseSchemaToken objectInstance
                Required = parseRequired objectInstance
                AdditionalProperties = parseAdditionalProperties objectInstance }