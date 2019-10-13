module ParserTests2

open System
open Xunit
open Swensen.Unquote
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.Parser
open Hedgehog

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
        | JsonElementSchema.Null _ -> JProperty("type", "null") |> Seq.singleton
        | JsonElementSchema.Boolean _ -> JProperty("type", "boolean") |> Seq.singleton
        | JsonElementSchema.Number _ -> JProperty("type", "number") |> Seq.singleton
        | JsonElementSchema.String stringSchema -> mapStringToJProperties stringSchema
        | JsonElementSchema.Object objectSchema -> mapObjectToJProperties objectSchema
        | _ -> raise (Exception ("Unhandled"))
        |> JObject :> JToken

    let serialize schema = mapToJToken schema |> JsonConvert.SerializeObject

[<Fact>]
let ``deserialization does not lose entropy`` () =
    Property.check <| property {
        let! schema = Gen.Schema.Defaults.jsonElement
        let roundTrippedSchema = schema |> JsonElementSchema.serialize |> parse
        test <@ Ok schema = roundTrippedSchema @> }