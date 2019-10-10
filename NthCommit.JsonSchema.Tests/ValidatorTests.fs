module ValidatorTests

open System
open Hedgehog
open Swensen.Unquote
open Xunit
open NthCommit.JsonSchema
open NthCommit.JsonSchema.Dom
open Newtonsoft.Json.Linq

module private Result =

    let isOk result =
        match result with
        | Ok _ -> true
        | Error _ -> false

    let isError = isOk >> not

module Gen =

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

        let jsonObject jsonElement (degree, depth) = gen {
            let nextDepth = Range.decrement depth
            let jsonElement' = jsonElement (degree, nextDepth)

            let! properties = jsonObjectProperty jsonElement' |> Gen.list degree
    
            return JsonSchemaElement.Object {
                Properties = properties
                PatternProperties = []
                Required = []
                AdditionalProperties = true } }

        let rec jsonElement (degree, depth) : Gen<JsonSchemaElement> = Gen.choice [
            jsonNull
            jsonNumber
            jsonString
            jsonObject jsonElement (degree, depth) ]

        let jsonElementOfType (degree, depth) primitive =
            jsonElement (degree, depth)
            |> Gen.filter (fun x -> x.Primitive = primitive)

        let jsonElementNotOfType (degree, depth) primitive =
            jsonElement (degree, depth)
            |> Gen.filter (fun x -> x.Primitive <> primitive)

        module Mutations =

            let private runMutation (mutator : JsonSchemaElement -> Option<Gen<JsonSchemaElement>>) element = seq {
                match mutator element with
                | Some mutation -> yield mutation
                | None _ -> () }

            let rec private mutateProperty mutator (spec : JsonSchemaObjectProperty) =
                match spec with
                | Inline (propertyName, element) ->
                    mutateElement mutator element
                    |> Seq.map (Gen.map (fun element' -> Inline (propertyName, element')))
                | Reference _ -> Seq.empty

            and private mutateObject mutator (spec : JsonSchemaObject) : seq<Gen<JsonSchemaObject>> = seq {
                yield!
                    spec.Properties
                    |> List.indexed
                    |> List.map (fun (i, propertySpec) ->
                        propertySpec
                        |> mutateProperty mutator
                        |> Seq.map (Gen.map (fun propertySpec' ->
                            let rebuiltProperties =
                                spec.Properties
                                |> List.indexed
                                |> List.map (fun (j, p) -> if i = j then propertySpec' else p)
                            { spec with Properties = rebuiltProperties })))
                    |> Seq.concat }

            and private recursivelyRunMutation (mutator : JsonSchemaElement -> Option<Gen<JsonSchemaElement>>) element =
                match element with
                | JsonSchemaElement.Object spec ->
                    mutateObject mutator spec
                    |> Seq.map (Gen.map (JsonSchemaElement.Object))
                | _ -> Seq.empty

            and mutateElement
                (mutator : JsonSchemaElement -> Option<Gen<JsonSchemaElement>>)
                (element : JsonSchemaElement) : seq<Gen<JsonSchemaElement>> = seq {
                    yield! runMutation mutator element
                    yield! recursivelyRunMutation mutator element }

            let tryMutateSchema (element : JsonSchemaElement) (mutator : JsonSchemaElement -> Option<Gen<JsonSchemaElement>>) = gen {
                let mutations = mutateElement mutator element
                let! mutationGenerator = Gen.item mutations
                return! mutationGenerator }

            let mutateSchema element mutator = tryMutateSchema element (mutator >> Some)

    module Instance =

        type InstanceGenOptions = {
            GenerateAllProperties : bool }

        let rec private concat (gens : List<Gen<'a>>) : Gen<List<'a>> = gen {
            match gens with
            | [] -> return []
            | x :: xs ->
                let! x' = x
                let! xs' = concat xs
                return x' :: xs' }

        let private shuffle (xs: 'a list) = gen {
            let shuffled = Array.zeroCreate<'a>(xs.Length)
            for i = 0 to xs.Length - 1 do
                let! j = Gen.integral (Range.constant 0 i)
                if i <> j then shuffled.[i] <- shuffled.[j]
                shuffled.[j] <- xs.[i]
            return shuffled |> Array.toList }

        let private manyOrNoItems (list : 'a list) : Gen<List<'a>> = gen {
            let! desiredLength = Gen.int (Range.linear 0 (list |> List.length))
            let! shuffled = shuffle list
            return shuffled |> List.take desiredLength }

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

        let private jsonObject json (options : InstanceGenOptions) (spec : JsonSchemaObject) : Gen<JToken> = gen {
            let! propertySpecs =
                if options.GenerateAllProperties
                then spec.Properties |> shuffle
                else spec.Properties |> manyOrNoItems
            let! properties =
                propertySpecs
                |> List.map (jsonProperty json)
                |> concat
            return JObject(properties) :> JToken }

        let rec private jsonElement (options : InstanceGenOptions) (schema : JsonSchemaElement) : Gen<JToken> =
            match schema with
            | JsonSchemaElement.Null -> createJValue null |> Gen.constant
            | JsonSchemaElement.Number -> jsonNumber
            | JsonSchemaElement.String spec -> jsonString spec
            | JsonSchemaElement.Object spec -> jsonObject (jsonElement options) (options) spec
            | _ -> raise (Exception ("Unhandled"))

        let json options schema = jsonElement options schema |> Gen.Json.serialize

        let jsonDefault = json { GenerateAllProperties = false }

open Gen.Instance

let DEFAULT_SCHEMA_RANGE = ((Range.exponential 0 6), (Range.linear 1 6))

[<Fact>]
let ``premise: test can generate a valid json instance`` () =
    Property.check <| property {
        let! schema = Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
        let! instance = Gen.Instance.jsonDefault schema
        test <@ Validator.validate schema instance |> Result.isOk @> }

let mutateDocumentType (schema : JsonSchemaElement) =
    Gen.Schema.jsonElementNotOfType DEFAULT_SCHEMA_RANGE schema.Primitive

[<Fact>]
let ``validation fails when type of schema doesn't match type of instance`` () =
    Property.check <| property {
        let! schema = Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
        let! schema' = Gen.Schema.Mutations.mutateSchema schema mutateDocumentType
        let! instance = Gen.Instance.json { GenerateAllProperties = true } schema'
        test <@ Validator.validate schema instance |>  Result.isError @> }

module Strings =

    let propertyContainsSchemaWhere (predicate : JsonSchemaElement -> bool) = function
        | Inline (_, schema) -> predicate schema
        | Reference _ -> false // The reference should be traversed elsewhere

    let rec schemaDefinesStringWhere (predicate : JsonSchemaString -> bool) = function
        | JsonSchemaElement.String spec -> predicate spec
        | JsonSchemaElement.Object spec ->
            spec.Properties
            |> List.map (propertyContainsSchemaWhere <| schemaDefinesStringWhere predicate)
            |> List.exists id
        | _ -> false

    let stringIsConst = function
        | JsonSchemaString.Const _ -> true
        | _ -> false

    let stringIsEnum = function
    | JsonSchemaString.Enum _ -> true
    | _ -> false

    let tryMutateString (mutator : JsonSchemaString -> Option<Gen<JsonSchemaElement>>) = function
        | JsonSchemaElement.String spec -> mutator spec
        | _ -> None

    let tryMutateConstStringSpec = function
        | JsonSchemaString.Const value ->
            Gen.Schema.jsonConstStringNotEqualTo value
            |> Some
        | _ -> None

    let tryMutateEnumStringSpec = function
        | JsonSchemaString.Enum values ->
            Gen.Schema.jsonEnumStringNotIntersectingWith values
            |> Some
        | _ -> None

    [<Fact>]
    let ``validation fails when string is not equal to const specification`` () =
        Property.check <| property {
            let! schema =
                Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
                |> Gen.filter (schemaDefinesStringWhere stringIsConst)

            let! schema' =
                tryMutateString tryMutateConstStringSpec
                |> Gen.Schema.Mutations.tryMutateSchema schema

            let! instance = Gen.Instance.json { GenerateAllProperties = true } schema'
            test <@ Validator.validate schema instance |>  Result.isError @> }

    [<Fact>]
    let ``validation fails when string is not in enum specification`` () =
        Property.check <| property {
            let! schema =
                Gen.Schema.jsonElement DEFAULT_SCHEMA_RANGE
                |> Gen.filter (schemaDefinesStringWhere stringIsEnum)

            let! schema' =
                tryMutateString tryMutateEnumStringSpec
                |> Gen.Schema.Mutations.tryMutateSchema schema

            let! instance = Gen.Instance.json { GenerateAllProperties = true } schema'
            test <@ Validator.validate schema instance |>  Result.isError @> }
