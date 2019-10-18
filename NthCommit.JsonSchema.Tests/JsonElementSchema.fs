module JsonElementSchema

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Domain

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

and private mapArrayToJProperties (arraySchema : JsonArraySchema) = seq {
    yield JProperty("type", "array")
    yield JProperty("items", mapToJToken arraySchema.Items) }

and private mapToJToken (schema : JsonElementSchema) : JToken =
    match schema with
    | JsonElementSchema.Null _ -> JProperty("type", "null") |> Seq.singleton
    | JsonElementSchema.Boolean _ -> JProperty("type", "boolean") |> Seq.singleton
    | JsonElementSchema.Number _ -> JProperty("type", "number") |> Seq.singleton
    | JsonElementSchema.String stringSchema -> mapStringToJProperties stringSchema
    | JsonElementSchema.Array arraySchema -> mapArrayToJProperties arraySchema
    | JsonElementSchema.Object objectSchema -> mapObjectToJProperties objectSchema
    | JsonElementSchema.Unvalidated _ -> Seq.empty
    |> JObject :> JToken

let serialize schema = mapToJToken schema |> JsonConvert.SerializeObject

let rec private mapPropertySchemas (f : JsonElementSchema -> JsonElementSchema) = function
    | JsonPropertySchema.Inline (name, schema) ->
        JsonPropertySchema.Inline (name, mapElements f schema)
    | x -> x

and private mapObjectSchemas (f : JsonElementSchema -> JsonElementSchema) (x : JsonObjectSchema) =
    { x with
        Properties = List.map (mapPropertySchemas f) x.Properties } : JsonObjectSchema

and private mapArraySchemas (f : JsonElementSchema -> JsonElementSchema) (x : JsonArraySchema) =
    { x with
        Items = f x.Items } : JsonArraySchema

and mapElements (f : JsonElementSchema -> JsonElementSchema) x =
    match f x with
    | JsonElementSchema.Object x ->
        mapObjectSchemas f x |> JsonElementSchema.Object
    | JsonElementSchema.Array x ->
        mapArraySchemas f x |> JsonElementSchema.Array
    | x -> x

let mapObjects (f : JsonObjectSchema -> JsonObjectSchema) = mapElements (function
    | JsonElementSchema.Object x -> f x |> JsonElementSchema.Object
    | x -> x)

let rec private existsInProperty (predicate : JsonElementSchema -> bool) = function
    | JsonPropertySchema.Inline (_, schema) -> exists predicate schema
    | _ -> false

and private existsInObject (predicate : JsonElementSchema -> bool) (spec : JsonObjectSchema) =
    List.exists (existsInProperty predicate) spec.Properties

and private existsInArray (predicate : JsonElementSchema -> bool) (spec : JsonArraySchema) =
    predicate spec.Items

and private existsNested (predicate : JsonElementSchema -> bool) = function
    | JsonElementSchema.Object objectSchema -> existsInObject predicate objectSchema
    | JsonElementSchema.Array arraySchema -> existsInArray predicate arraySchema
    | _ -> false

and exists (predicate : JsonElementSchema -> bool) (element : JsonElementSchema) =
    predicate element || existsNested predicate element : bool

let stringExists (predicate : JsonStringSchema -> bool) = exists (function
    | JsonElementSchema.String objectSchema -> predicate objectSchema
    | _ -> false)

let objectExists (predicate : JsonObjectSchema -> bool) = exists (function
    | JsonElementSchema.Object objectSchema -> predicate objectSchema
    | _ -> false)