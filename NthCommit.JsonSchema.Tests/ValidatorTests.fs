module ValidatorTests

open System
open Hedgehog
open Swensen.Unquote
open Xunit
open NthCommit.JsonSchema.Dom
open Newtonsoft.Json.Linq
open FSharpx.Collections
open NthCommit.JsonSchema.Validation

module JsonElementSchema =

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

module Gen =

    module Schema =

        module Mutations =

            let rec private runMutatorForProperty mutator (propertySchema : JsonPropertySchema) =
                match propertySchema with
                | Inline (propertyName, element) ->
                    runMutatorForElement mutator element
                    |> Seq.map (Gen.map (fun element' -> Inline (propertyName, element')))
                | Reference _ -> Seq.empty

            and private runMutatorForObject mutator (objectSchema : JsonObjectSchema) : seq<Gen<JsonObjectSchema>> = seq {
                yield!
                    objectSchema.Properties
                    |> List.indexed
                    |> List.map (fun (i, propertySpec) ->
                        propertySpec
                        |> runMutatorForProperty mutator
                        |> Seq.map (Gen.map (fun propertySpec' ->
                            let rebuiltProperties =
                                objectSchema.Properties
                                |> List.indexed
                                |> List.map (fun (j, p) -> if i = j then propertySpec' else p)
                            { objectSchema with Properties = rebuiltProperties })))
                    |> Seq.concat }

            and private runMutatorForNestedElements (mutator : JsonElementSchema -> seq<Gen<JsonElementSchema>>) element =
                match element with
                | JsonElementSchema.Array arraySchema ->
                    mutator arraySchema.Items
                    |> Seq.map (Gen.map (fun items -> JsonElementSchema.Array { Items = items }))
                | JsonElementSchema.Object objectSchema ->
                    runMutatorForObject mutator objectSchema
                    |> Seq.map (Gen.map (JsonElementSchema.Object))
                | _ -> Seq.empty

            and private runMutatorForElement
                (mutator : JsonElementSchema -> seq<Gen<JsonElementSchema>>)
                (element : JsonElementSchema) : seq<Gen<JsonElementSchema>> = seq {
                    yield! mutator element
                    yield! runMutatorForNestedElements mutator element }

            let mutateElement (mutator : JsonElementSchema -> seq<Gen<JsonElementSchema>>) (element : JsonElementSchema) = gen {
                let mutationGenerators = runMutatorForElement mutator element
                let! mutationGenerator = Gen.item mutationGenerators |> Gen.noShrink
                let! mutation = mutationGenerator |> Gen.noShrink
                if element = mutation then raise (Exception("Mutation failed"))
                return mutation }

            let mutateString (mutator : JsonStringSchema -> seq<Gen<JsonElementSchema>>) (element : JsonElementSchema) =
                let mutator' = function
                    | JsonElementSchema.String stringSchema -> mutator stringSchema
                    | _ -> Seq.empty
                mutateElement mutator' element

            let mutateObject (mutator : JsonObjectSchema -> seq<Gen<JsonElementSchema>>) (element : JsonElementSchema) =
                let mutator' = function
                    | JsonElementSchema.Object objectSchema -> mutator objectSchema
                    | _ -> Seq.empty
                mutateElement mutator' element

            let tryMutateElement (mutator : JsonElementSchema -> seq<Gen<JsonElementSchema>>) (element : JsonElementSchema) = gen {
                let mutations = runMutatorForElement mutator element
                if mutations |> Seq.isEmpty
                then return None
                else
                    let! mutationGenerator = Gen.item mutations |> Gen.noShrink
                    return! mutationGenerator |> Gen.map Some |> Gen.noShrink }

            let tryMutateString (mutator : JsonStringSchema -> seq<Gen<JsonElementSchema>>) (element : JsonElementSchema) =
                let mutator' = function
                    | JsonElementSchema.String stringSchema -> mutator stringSchema
                    | _ -> Seq.empty
                tryMutateElement mutator' element

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

        let private jsonString (stringSchema : JsonStringSchema) =
            match stringSchema with
            | JsonStringSchema.Const str -> Gen.constant str
            | JsonStringSchema.Enum values -> Gen.item values
            | JsonStringSchema.Unvalidated -> Gen.Strings.defaultString
            |> Gen.map createJValue

        let private jsonProperty (json : JsonElementSchema -> Gen<JToken>) (propertySchema : JsonPropertySchema) : Gen<JProperty> = gen {
            match propertySchema with
            | Inline (propertyName, schema) ->
                let! propertyValue = json schema
                return JProperty(propertyName, propertyValue)
            | _ ->
                raise (Exception("unhandled"))
                return JProperty(null) }

        let private pickProperties (objectSchema : JsonObjectSchema) : Gen<List<JsonPropertySchema>> =
            let (required, unrequired) =
                objectSchema.Properties
                |> List.partition (fun p -> objectSchema.Required |> Set.contains p.Name)
            Gen.map2
                (@)
                (required |> Gen.shuffle)
                (unrequired |> Gen.manyOrNoItems)

        let private jsonObject json (objectSchema : JsonObjectSchema) : Gen<JToken> = gen {
            let! propertySpecs = pickProperties objectSchema
            let! properties =
                propertySpecs
                |> List.map (jsonProperty json)
                |> concat
            return JObject(properties) :> JToken }

        let private jsonArray (json : JsonElementSchema -> Gen<JToken>) (arraySchema : JsonArraySchema) : Gen<JToken> =
            json arraySchema.Items
            |> Gen.list (Range.exponential 1 10) // TODO: Use the schema property minItems, but for now always generate at least one element
            |> Gen.map (fun items -> JArray(items) :> JToken)

        let rec private jsonElement (schema : JsonElementSchema) : Gen<JToken> =
            match schema with
            | JsonElementSchema.Null -> createJValue null |> Gen.constant
            | JsonElementSchema.Boolean -> Gen.bool |> Gen.map createJValue
            | JsonElementSchema.Number -> jsonNumber
            | JsonElementSchema.String stringSchema -> jsonString stringSchema
            | JsonElementSchema.Array arraySchema -> jsonArray jsonElement arraySchema
            | JsonElementSchema.Object objectSchema -> jsonObject jsonElement objectSchema
            | JsonElementSchema.Unvalidated -> JObject.FromObject({||}) :> JToken |> Gen.constant

        let json schema = jsonElement schema |> Gen.Json.serialize

let allPropertiesAreRequired (objectSchema : JsonObjectSchema) =
    { objectSchema with Required = objectSchema.Properties |> List.map (fun p -> p.Name) |> Set }

let validate schema instance =
    match validate schema instance with
    | Ok _ -> []
    | Error errors -> errors

[<Fact>]
let ``premise: test can generate a valid json instance`` () =
    Property.check <| property {
        let! schema = Gen.Schema.Defaults.jsonElement
        let! instance = Gen.Instance.json schema
        test <@ validate schema instance = [] @> }

let mutateDocumentType (schema : JsonElementSchema) =
    Gen.Schema.Defaults.jsonElementNotOfType schema.Primitive
    |> Seq.singleton

[<Fact>]
let ``validation fails when type of schema doesn't match type of instance`` () =
    Property.check <| property {
        let! schema =
            Gen.Schema.Defaults.jsonElement
            |> Gen.map (JsonElementSchema.mapObjects allPropertiesAreRequired)

        let! schema' =
            schema
            |> Gen.Schema.Mutations.mutateElement mutateDocumentType

        let! instance = Gen.Instance.json schema'
        test <@ validate schema instance <> [] @> }

module Strings =

    let stringIsConst = function
        | JsonStringSchema.Const _ -> true
        | _ -> false

    let replaceStringConstValue = function
        | JsonStringSchema.Const value ->
            Gen.Schema.jsonConstStringNotEqualTo value
            |> Seq.singleton
        | _ -> Seq.empty

    [<Fact>]
    let ``validating string const`` () =
        Property.check <| property {
            let! schema =
                Gen.Schema.Defaults.jsonElement
                |> Gen.filter (JsonElementSchema.stringExists stringIsConst)
                |> Gen.map (JsonElementSchema.mapObjects allPropertiesAreRequired)

            let! schema' =
                schema
                |> Gen.Schema.Mutations.mutateString replaceStringConstValue

            let! instance = Gen.Instance.json schema'
            test <@ validate schema instance <> [] @> }

    let stringIsEnum = function
        | JsonStringSchema.Enum _ -> true
        | _ -> false

    let replaceStringEnumValues = function
        | JsonStringSchema.Enum values ->
            Gen.Schema.jsonEnumStringNotIntersectingWith values
            |> Seq.singleton
        | _ -> Seq.empty

    [<Fact>]
    let ``validating string enum`` () =
        Property.check <| property {
            let! schema =
                Gen.Schema.Defaults.jsonElement
                |> Gen.filter (JsonElementSchema.stringExists stringIsEnum)
                |> Gen.map (JsonElementSchema.mapObjects allPropertiesAreRequired)

            let! schema' =
                schema
                |> Gen.Schema.Mutations.mutateString replaceStringEnumValues

            let! instance = Gen.Instance.json schema'
            test <@ validate schema instance <> [] @> }

module Objects =

    let hasPropertyDefinition (objectSchema : JsonObjectSchema) =
        objectSchema.Properties
        |> (not << List.isEmpty)

    let clearPropertyDefinitions (objectSchema : JsonObjectSchema) =
        if objectSchema.Properties |> List.isEmpty
        then Seq.empty
        else
            { objectSchema with Properties = []; Required = Set []}
            |> JsonElementSchema.Object
            |> Gen.constant
            |> Seq.singleton

    [<Fact>]
    let ``validating required properties`` () =
        Property.check <| property {
            let! schema =
                Gen.Schema.Defaults.jsonObject
                |> Gen.filter (JsonElementSchema.objectExists (hasPropertyDefinition))
                |> Gen.map (JsonElementSchema.mapObjects allPropertiesAreRequired)

            let! schema' =
                schema
                |> Gen.Schema.Mutations.mutateObject clearPropertyDefinitions

            let! instance = Gen.Instance.json schema'
            test <@ validate schema instance <> [] @> }

    let setAdditionalProperties value (objectSchema : JsonObjectSchema) =
        { objectSchema with AdditionalProperties = value }

    let addPropertyDefinitions = fun (objectSchema : JsonObjectSchema) ->
        gen {
            let! additionalProperties =
                Gen.Schema.jsonObjectProperty Gen.Schema.jsonUnvalidated
                |> Gen.notInBy objectSchema.Properties (fun p -> p.Name)
                |> Gen.listDistinct (Range.exponential 1 10)
            let newProperties = List.append objectSchema.Properties additionalProperties
            return
                { objectSchema with Properties = newProperties }
                |> JsonElementSchema.Object
        }
        |> Seq.singleton

    [<Theory>]
    [<InlineData(true)>]
    [<InlineData(false)>]
    let ``validating additionalProperties`` (additionalProperties) =
        Property.check <| property {
            let! schema =
                Gen.Schema.Defaults.jsonObject
                |> Gen.map (JsonElementSchema.mapObjects (setAdditionalProperties additionalProperties))

            let! schema' =
                schema
                |> Gen.Schema.Mutations.mutateObject addPropertyDefinitions
                |> Gen.map (JsonElementSchema.mapObjects allPropertiesAreRequired)

            let! instance = Gen.Instance.json schema'
            if additionalProperties
            then test <@ validate schema instance = [] @>
            else test <@ validate schema instance <> [] @> }