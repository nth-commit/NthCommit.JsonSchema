module ParserTests2

open System
open Xunit
open Swensen.Unquote
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.Parser

module JsonElementSchema =

    let private mapStringToJProperties (stringSchema : JsonStringSchema) = seq {
        yield JProperty("type", "string")
        match stringSchema with
        | JsonStringSchema.Unvalidated -> ()
        | JsonStringSchema.Const value -> yield JProperty("const", value)
        | JsonStringSchema.Enum values -> yield JProperty("enum", values) }

    let rec private mapPropertyToJProperty (propertySchema : JsonPropertySchema) =
        match propertySchema with
        | Inline (name, schema) -> JProperty(name, mapToJToken schema)
        | Reference (JsonReference ref) -> JProperty("$ref", ref)

    and private mapPropertiesToJObject (propertySchemas : JsonPropertySchema list) =
        propertySchemas
        |> Seq.map mapPropertyToJProperty
        |> JObject

    and private mapObjectToJProperties (objectSchema : JsonObjectSchema) = seq {
        yield JProperty("type", "object")

        match objectSchema.Properties with
        | [] -> ()
        | properties ->
            yield JProperty("properties", mapPropertiesToJObject properties)

        match objectSchema.PatternProperties with
        | [] -> ()
        | patternProperties ->
            let patternPropertiesAsJObject =
                patternProperties
                |> Seq.map (fun ((RegularExpression pattern), properties) ->
                    JProperty(pattern, mapPropertiesToJObject properties))
                |> JObject
            yield JProperty("patternProperties", patternPropertiesAsJObject)

        match objectSchema.Required |> Set.toList with
        | [] -> ()
        | required -> yield JProperty("required", required)

        yield JProperty("additionalProperties", objectSchema.AdditionalProperties |> JValue) }

    and private mapToJToken (schema : JsonElementSchema) : JToken =
        match schema with
        | JsonElementSchema.String stringSchema -> mapStringToJProperties stringSchema |> JObject :> JToken
        | JsonElementSchema.Object objectSchema -> mapObjectToJProperties objectSchema |> JObject :> JToken
        | x -> null

    let serialize schema = mapToJToken schema |> JsonConvert.SerializeObject

let raiseArrogantException () =
    raise (Exception ("No way will this fail"))

let parseArrogantly schemaJson =
    match parse schemaJson with
    | Ok schema -> schema
    | Error _ -> raiseArrogantException ()

[<Fact>]
let ``try serialize something`` () =
    let roundTripped = META_SCHEMA |> JsonElementSchema.serialize |> parseArrogantly
    test <@ META_SCHEMA = roundTripped @>