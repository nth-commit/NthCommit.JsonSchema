﻿module ValidatorTests

open System
open Hedgehog
open Swensen.Unquote
open Xunit
open NthCommit.JsonSchema
open NthCommit.JsonSchema.Dom
open Newtonsoft.Json.Linq
open FSharpx.Collections

module JsonSchemaElement =

    let rec private mapPropertySchemas (f : JsonSchemaElement -> JsonSchemaElement) = function
        | JsonSchemaObjectProperty.Inline (name, schema) ->
            JsonSchemaObjectProperty.Inline (name, map f schema)
        | x -> x

    and private mapObjectSchemas (f : JsonSchemaElement -> JsonSchemaElement) (x : JsonSchemaObject) =
        { x with
            Properties = List.map (mapPropertySchemas f) x.Properties } : JsonSchemaObject

    and private mapArraySchemas (f : JsonSchemaElement -> JsonSchemaElement) (x : JsonSchemaArray) =
        { x with
            Items = f x.Items } : JsonSchemaArray

    and map (f : JsonSchemaElement -> JsonSchemaElement) x =
        match f x with
        | JsonSchemaElement.Object x ->
            mapObjectSchemas f x |> JsonSchemaElement.Object
        | JsonSchemaElement.Array x ->
            mapArraySchemas f x |> JsonSchemaElement.Array
        | x -> x

    let mapObjects (f : JsonSchemaObject -> JsonSchemaObject) = map (function
        | JsonSchemaElement.Object x -> f x |> JsonSchemaElement.Object
        | x -> x)

    let rec private existsInProperty (predicate : JsonSchemaElement -> bool) = function
        | JsonSchemaObjectProperty.Inline (_, schema) -> exists predicate schema
        | _ -> false

    and private existsInObject (predicate : JsonSchemaElement -> bool) (spec : JsonSchemaObject) =
        List.exists (existsInProperty predicate) spec.Properties

    and private existsInArray (predicate : JsonSchemaElement -> bool) (spec : JsonSchemaArray) =
        predicate spec.Items

    and private existsNested (predicate : JsonSchemaElement -> bool) = function
        | JsonSchemaElement.Object spec -> existsInObject predicate spec
        | JsonSchemaElement.Array spec -> existsInArray predicate spec
        | _ -> false

    and exists (predicate : JsonSchemaElement -> bool) (element : JsonSchemaElement) =
        predicate element || existsNested predicate element : bool

    let stringExists (predicate : JsonSchemaString -> bool) = exists (function
        | JsonSchemaElement.String spec -> predicate spec
        | _ -> false)

module Gen =

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

    module private Range =

        let decrement range =
            range
            |> Range.map (fun i -> i - 1)

    module Schema =

        let jsonNull = Gen.constant JsonSchemaElement.Null

        let jsonNumber = Gen.constant JsonSchemaElement.Number

        let jsonEnumStringNotIntersectingWith (values : Set<string>) =
            Gen.Strings.defaultString
            |> Gen.list (Range.linear 1 20)
            |> Gen.map (Set)
            |> Gen.filter (Set.intersect values >> Set.count >> (=) 0)
            |> Gen.map (Set >> JsonSchemaString.Enum >> JsonSchemaElement.String)

        let jsonEnumString =
            jsonEnumStringNotIntersectingWith (Set [])

        let jsonConstStringNotEqualTo value =
            Gen.Strings.defaultString
            |> Gen.filter ((<>) value)
            |> Gen.map (JsonSchemaString.Const >> JsonSchemaElement.String)

        let jsonConstString =
            Gen.Strings.defaultString
            |> Gen.map (JsonSchemaString.Const >> JsonSchemaElement.String)

        let jsonString =
            Gen.choice [
                jsonEnumString
                jsonConstString
                Gen.constant JsonSchemaString.Unvalidated |> Gen.map JsonSchemaElement.String ]

        let jsonObjectInlineProperty jsonSchemaDocument = gen {
            let! propertyName = Gen.Strings.camelCaseWord
            let! propertyValue = jsonSchemaDocument
            return JsonSchemaObjectProperty.Inline (propertyName, propertyValue) }

        let jsonObjectProperty jsonSchemaDocument =
            Gen.choice [
                jsonObjectInlineProperty jsonSchemaDocument ]

        let rec jsonObject (degree, depth) = gen {
            let nextDepth = Range.decrement depth
            let jsonElement' = jsonElement (degree, nextDepth)

            let! properties =
                jsonObjectProperty jsonElement'
                |> listDistinctBy (fun p -> p.Name) degree

            let! requiredProperties =
                properties
                |> List.map (fun p -> p.Name)
                |> manyOrNoItems
                |> Gen.map Set
    
            return JsonSchemaElement.Object {
                Properties = properties
                PatternProperties = []
                Required = requiredProperties
                AdditionalProperties = true } }

        and jsonElement (degree, depth) : Gen<JsonSchemaElement> = Gen.choice [
            jsonNull
            jsonNumber
            jsonString
            jsonObject (degree, depth) ]

        let jsonElementOfType (degree, depth) primitive =
            jsonElement (degree, depth)
            |> Gen.filter (fun x -> x.Primitive = primitive)

        let jsonElementNotOfType (degree, depth) primitive =
            jsonElement (degree, depth)
            |> Gen.filter (fun x -> x.Primitive <> primitive)

        module Mutations =

            let rec private runMutatorForProperty mutator (spec : JsonSchemaObjectProperty) =
                match spec with
                | Inline (propertyName, element) ->
                    runMutatorForElement mutator element
                    |> Seq.map (Gen.map (fun element' -> Inline (propertyName, element')))
                | Reference _ -> Seq.empty

            and private runMutatorForObject mutator (spec : JsonSchemaObject) : seq<Gen<JsonSchemaObject>> = seq {
                yield!
                    spec.Properties
                    |> List.indexed
                    |> List.map (fun (i, propertySpec) ->
                        propertySpec
                        |> runMutatorForProperty mutator
                        |> Seq.map (Gen.map (fun propertySpec' ->
                            let rebuiltProperties =
                                spec.Properties
                                |> List.indexed
                                |> List.map (fun (j, p) -> if i = j then propertySpec' else p)
                            { spec with Properties = rebuiltProperties })))
                    |> Seq.concat }

            and private runMutatorForNestedElements (mutator : JsonSchemaElement -> seq<Gen<JsonSchemaElement>>) element =
                match element with
                | JsonSchemaElement.Object spec ->
                    runMutatorForObject mutator spec
                    |> Seq.map (Gen.map (JsonSchemaElement.Object))
                | _ -> Seq.empty

            and private runMutatorForElement
                (mutator : JsonSchemaElement -> seq<Gen<JsonSchemaElement>>)
                (element : JsonSchemaElement) : seq<Gen<JsonSchemaElement>> = seq {
                    yield! mutator element
                    yield! runMutatorForNestedElements mutator element }

            let mutateElement (mutator : JsonSchemaElement -> seq<Gen<JsonSchemaElement>>) (element : JsonSchemaElement) = gen {
                let mutations = runMutatorForElement mutator element
                let! mutationGenerator = Gen.item mutations |> Gen.noShrink
                return! mutationGenerator |> Gen.noShrink }

            let mutateString (mutator : JsonSchemaString -> seq<Gen<JsonSchemaElement>>) (element : JsonSchemaElement) =
                let mutator' = function
                    | JsonSchemaElement.String spec -> mutator spec
                    | _ -> Seq.empty
                mutateElement mutator' element

            let mutateObject (mutator : JsonSchemaObject -> seq<Gen<JsonSchemaElement>>) (element : JsonSchemaElement) =
                let mutator' = function
                    | JsonSchemaElement.Object spec -> mutator spec
                    | _ -> Seq.empty
                mutateElement mutator' element

    module Instance =

        let rec private concat (gens : List<Gen<'a>>) : Gen<List<'a>> = gen {
            match gens with
            | [] -> return []
            | x :: xs ->
                let! x' = x
                let! xs' = concat xs
                return x' :: xs' }

        let private createJValue (x : obj) = JValue(x) :> JToken

        let private jsonNumber =
            Gen.int (Range.linear -1000 1000)
            |> Gen.map createJValue

        let private jsonString (spec : JsonSchemaString) =
            match spec with
            | JsonSchemaString.Const str -> Gen.constant str
            | JsonSchemaString.Enum values -> Gen.item values
            | JsonSchemaString.Unvalidated -> Gen.Strings.defaultString
            |> Gen.map createJValue

        let private jsonProperty (json : JsonSchemaElement -> Gen<JToken>) (spec : JsonSchemaObjectProperty) : Gen<JProperty> = gen {
            match spec with
            | Inline (propertyName, schema) ->
                let! propertyValue = json schema
                return JProperty(propertyName, propertyValue)
            | _ ->
                raise (Exception("unhandled"))
                return JProperty(null) }

        let private pickProperties (spec : JsonSchemaObject) : Gen<List<JsonSchemaObjectProperty>> =
            let (required, unrequired) =
                spec.Properties
                |> List.partition (fun p -> spec.Required |> Set.contains p.Name)
            Gen.map2
                (@)
                (required |> shuffle)
                (unrequired |> manyOrNoItems)

        let private jsonObject json (spec : JsonSchemaObject) : Gen<JToken> = gen {
            let! propertySpecs = pickProperties spec
            let! properties =
                propertySpecs
                |> List.map (jsonProperty json)
                |> concat
            return JObject(properties) :> JToken }

        let rec private jsonElement (schema : JsonSchemaElement) : Gen<JToken> =
            match schema with
            | JsonSchemaElement.Null -> createJValue null |> Gen.constant
            | JsonSchemaElement.Number -> jsonNumber
            | JsonSchemaElement.String spec -> jsonString spec
            | JsonSchemaElement.Object spec -> jsonObject jsonElement spec
            | _ -> raise (Exception ("Unhandled"))

        let json schema = jsonElement schema |> Gen.Json.serialize

let DEFAULT_SCHEMA_RANGE = ((Range.constant 0 6), (Range.linear 1 6))

let allPropertiesAreRequired (spec : JsonSchemaObject) =
    { spec with Required = spec.Properties |> List.map (fun p -> p.Name) |> Set }

let validate schema instance =
    match Validator.validate schema instance with
    | Ok _ -> []
    | Error errors -> errors

[<Fact>]
let ``premise: test can generate a valid json instance`` () =
    Property.check <| property {
        let! schema = Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
        let! instance = Gen.Instance.json schema
        test <@ validate schema instance |> List.isEmpty @> }

let mutateDocumentType (schema : JsonSchemaElement) =
    Gen.Schema.jsonElementNotOfType DEFAULT_SCHEMA_RANGE schema.Primitive
    |> Seq.singleton

[<Fact>]
let ``validation fails when type of schema doesn't match type of instance`` () =
    Property.check <| property {
        let! schema =
            Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
            |> Gen.map (JsonSchemaElement.mapObjects allPropertiesAreRequired)

        let! schema' =
            schema
            |> Gen.Schema.Mutations.mutateElement mutateDocumentType

        let! instance = Gen.Instance.json schema'
        test <@ validate schema instance <> [] @> }

module Strings =

    let stringIsConst = function
        | JsonSchemaString.Const _ -> true
        | _ -> false

    let replaceStringConstValue = function
        | JsonSchemaString.Const value ->
            Gen.Schema.jsonConstStringNotEqualTo value
            |> Seq.singleton
        | _ -> Seq.empty

    [<Fact>]
    let ``validation fails when string is not equal to const specification`` () =
        Property.check <| property {
            let! schema =
                Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
                |> Gen.filter (JsonSchemaElement.stringExists stringIsConst)
                |> Gen.map (JsonSchemaElement.mapObjects allPropertiesAreRequired)

            let! schema' =
                schema
                |> Gen.Schema.Mutations.mutateString replaceStringConstValue

            let! instance = Gen.Instance.json schema'
            test <@ validate schema instance <> [] @> }

    let stringIsEnum = function
        | JsonSchemaString.Enum _ -> true
        | _ -> false

    let replaceStringEnumValues = function
        | JsonSchemaString.Enum values ->
            Gen.Schema.jsonEnumStringNotIntersectingWith values
            |> Seq.singleton
        | _ -> Seq.empty

    [<Fact>]
    let ``validation fails when string is not in enum specification`` () =
        Property.check <| property {
            let! schema =
                Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
                |> Gen.filter (JsonSchemaElement.stringExists stringIsEnum)
                |> Gen.map (JsonSchemaElement.mapObjects allPropertiesAreRequired)

            let! schema' =
                schema
                |> Gen.Schema.Mutations.mutateString replaceStringEnumValues

            let! instance = Gen.Instance.json schema'
            test <@ validate schema instance <> [] @> }
