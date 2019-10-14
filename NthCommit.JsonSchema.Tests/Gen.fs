module Gen

open Hedgehog
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema
open System

let rec private listDistinctByInternal
    (projection : 'a -> 'b)
    (length : int)
    (generator : Gen<'a>)
    (prevResults : 'a list) : Gen<List<'a>> = gen {
        let! curr = generator
        let results =
            curr :: prevResults
            |> List.distinctBy projection
        if (results |> List.length) < length
        then return! listDistinctByInternal projection length generator results
        else return results }

let listDistinctBy
    (projection : 'a -> 'b)
    (range : Range<int>)
    (generator : Gen<'a>) : Gen<List<'a>> = gen {
        let! length = range |> Gen.integral
        return! listDistinctByInternal projection length generator [] }

let listDistinct (range : Range<int>) (generator : Gen<'a>) : Gen<List<'a>> =
    listDistinctBy id range generator

let shuffle (xs: 'a list) = gen {
    let shuffled = Array.zeroCreate<'a>(xs.Length)
    for i = 0 to xs.Length - 1 do
        let! j = Gen.integral (Range.constant 0 i)
        if i <> j then shuffled.[i] <- shuffled.[j]
        shuffled.[j] <- xs.[i]
    return shuffled |> Array.toList }

let manyOrNoItems (list : 'a list) : Gen<List<'a>> = gen {
    let! desiredLength = Gen.int (Range.linear 0 (list |> List.length))
    let! shuffled = shuffle list
    return shuffled |> List.take desiredLength }

module private Result =

    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    let isError r = not <| isOk r

module Strings =

    module private String =

        let capitalize (str : string) =
            match (str |> Seq.toList) with
            | [] -> ""
            | x :: xs ->
                x.ToString().ToUpper() :: (xs |> List.map string)
                |> List.reduce (+)

    let lowerCaseWord =
        ['a' .. 'z']
        |> Gen.item
        |> Gen.string (Range.exponential 1 100)

    let camelCaseWord =
        lowerCaseWord
        |> Gen.list (Range.linear 1 10)
        |> Gen.map (function
            | [] -> ""
            | x :: xs ->
                x :: (xs |> List.map String.capitalize)
                |> List.reduce (+))

    let defaultString = camelCaseWord

    let whitespace = gen {
        let whitespaceChar = [' '] |> Gen.item
        return! Gen.string (Range.exponential 0 1000) whitespaceChar }

module Json =

    let private tryDeserialize (json : string) : Result<'a, string> =
        try JsonConvert.DeserializeObject<'a>(json) |> Ok
        with | :? JsonException -> Error "Invalid JSON"
    
    let invalid =
        Gen.constant "not json"
        |> Gen.filter (tryDeserialize >> Result.isError)
    
    let valid =
        Gen.constant "{}"
        |> Gen.filter (tryDeserialize >> Result.isOk)

    let value : Gen<JValue> = [
        JValue.CreateNull() |> Gen.constant
        Gen.integral (Range.exponential -1000 1000) |> Gen.map (fun i -> JValue(i))
        Gen.bool |> Gen.map (fun b -> JValue(b))
        Strings.camelCaseWord |> Gen.map (fun s -> JValue(s)) ] |> Gen.choice

    let token = [
        value |> Gen.map (fun v -> v :> JToken)
        JArray.FromObject([]) |> Gen.constant |> Gen.map (fun a -> a :> JToken)
        JObject.FromObject({||}) |> Gen.constant |> Gen.map (fun o -> o :> JToken) ] |> Gen.choice

    let tokenOf (kind : JTokenType) =
        token
        |> Gen.filter (fun t -> t.Type = kind)

    let tokenNotOf (kind : JTokenType) =
        token
        |> Gen.filter (fun t -> t.Type <> kind)

    let stringValueNotOf strings =
        tokenOf JTokenType.String
        |> Gen.filter (fun t ->
            List.contains (t.Value<string>()) strings
            |> not)

    let propertyWithValue (name : Gen<string>) (value : Gen<JToken>) : Gen<JProperty> =
        Gen.zip name value
        |> Gen.map (fun (n, v) -> JProperty(n, v))

    let property name = propertyWithValue name token

    let objectOfOneProperty (property : Gen<JProperty>) : Gen<JObject> =
        property
        |> Gen.map (fun p -> JObject([p]))

    let serialize token =
        token
        |> Gen.map JsonConvert.SerializeObject

    let maybeAdditive (json : string) = gen {
        let jObject = JsonConvert.DeserializeObject<JObject> json
        let! newProperties =
            property Strings.camelCaseWord
            |> Gen.list (Range.exponential 0 50)
        let oldProperties = jObject.Properties() |> Seq.toList
        let mergedProperties =
            oldProperties @ newProperties
            |> List.distinctBy (fun p -> p.Name)
        return JsonConvert.SerializeObject(JObject (mergedProperties)) }

    let primitive =
        Gen.item [
            JsonPrimitive.Null
            JsonPrimitive.Boolean
            JsonPrimitive.Number
            JsonPrimitive.String
            JsonPrimitive.Array
            JsonPrimitive.Object ]

    let jsonOfPrimitive primitive =
        match primitive with
        | JsonPrimitive.Null    -> JTokenType.Null
        | JsonPrimitive.Boolean -> JTokenType.Boolean
        | JsonPrimitive.Number  -> JTokenType.Integer
        | JsonPrimitive.String  -> JTokenType.String
        | JsonPrimitive.Array   -> JTokenType.Array
        | JsonPrimitive.Object  -> JTokenType.Object
        |> tokenOf
        |> serialize

    let jsonNotOfPrimitive primitive =
        match primitive with
        | JsonPrimitive.Null    -> JTokenType.Null
        | JsonPrimitive.Boolean -> JTokenType.Boolean
        | JsonPrimitive.Number  -> JTokenType.Integer
        | JsonPrimitive.String  -> JTokenType.String
        | JsonPrimitive.Array   -> JTokenType.Array
        | JsonPrimitive.Object  -> JTokenType.Object
        |> tokenNotOf
        |> serialize

let notIn source generator =
    generator
    |> Gen.filter (fun x -> List.contains x source |> not)

let notInBy source projection generator =
    let projectedSource =
        source
        |> List.map projection
    generator
    |> Gen.filter (fun x ->
        let projectedX = projection x
        projectedSource
        |> List.contains projectedX
        |> not)

module Schema =

    open NthCommit.JsonSchema.Dom

    module private Range =

        let decrement range =
            range
            |> Range.map (fun i -> i - 1)

    let jsonUnvalidated = Gen.constant JsonElementSchema.Unvalidated

    let jsonNull = Gen.constant JsonElementSchema.Null

    let jsonNumber = Gen.constant JsonElementSchema.Number

    let jsonEnumStringNotIntersectingWith (values : Set<string>) =
        Strings.defaultString
        |> Gen.list (Range.linear 1 20)
        |> Gen.map (Set)
        |> Gen.filter (Set.intersect values >> Set.count >> (=) 0)
        |> Gen.map (Set >> JsonStringSchema.Enum >> JsonElementSchema.String)

    let jsonEnumString =
        jsonEnumStringNotIntersectingWith (Set [])

    let jsonConstStringNotEqualTo value =
        Strings.defaultString
        |> Gen.filter ((<>) value)
        |> Gen.map (JsonStringSchema.Const >> JsonElementSchema.String)

    let jsonConstString =
        Strings.defaultString
        |> Gen.map (JsonStringSchema.Const >> JsonElementSchema.String)

    let jsonString =
        Gen.choice [
            jsonEnumString
            jsonConstString
            Gen.constant JsonStringSchema.Unvalidated |> Gen.map JsonElementSchema.String ]

    let jsonObjectInlineProperty jsonSchemaDocument = gen {
        let! propertyName = Strings.camelCaseWord
        let! propertyValue = jsonSchemaDocument
        return JsonPropertySchema.Inline (propertyName, propertyValue) }

    let jsonObjectProperty jsonElement =
        Gen.choice [
            jsonObjectInlineProperty jsonElement ]

    let rec jsonObject (degree, depth) = gen {
        let! properties =
            jsonObjectProperty (jsonElement (degree, depth))
            |> listDistinctBy (fun p -> p.Name) degree

        let! requiredProperties =
            properties
            |> List.map (fun p -> p.Name)
            |> manyOrNoItems
            |> Gen.map Set

        let! additionalProperties = Gen.bool
    
        return JsonElementSchema.Object {
            Properties = properties
            PatternProperties = []
            Required = requiredProperties
            AdditionalProperties = additionalProperties } }

    and jsonArray (degree, depth) =
        jsonElement (degree, depth)
        |> Gen.map (fun schema -> JsonElementSchema.Array { Items = schema })

    and jsonElement (degree, depth) : Gen<JsonElementSchema> =
        Gen.sized <| fun size ->
            let maxDepth = Range.upperBound size depth
            seq {
                jsonNull
                jsonNumber
                jsonString
                if maxDepth > 0 then
                    jsonArray (degree, Range.decrement depth)
                    jsonObject (degree, Range.decrement depth) }
            |> Gen.choice

    let jsonElementOfType (degree, depth) primitive =
        jsonElement (degree, depth)
        |> Gen.filter (fun x -> x.Primitive = primitive)

    let jsonElementNotOfType (degree, depth) primitive =
        jsonElement (degree, depth)
        |> Gen.filter (fun x -> x.Primitive <> primitive)

    module Defaults = 

        let DEFAULT_SCHEMA_RANGE = ((Range.constant 0 6), (Range.linear 1 6))

        let jsonElement = jsonElement DEFAULT_SCHEMA_RANGE

        let jsonObject = jsonObject DEFAULT_SCHEMA_RANGE

        let jsonElementOfType primitive =
            jsonElement
            |> Gen.filter (fun x -> x.Primitive = primitive)

        let jsonElementNotOfType primitive =
            jsonElement
            |> Gen.filter (fun x -> x.Primitive <> primitive)