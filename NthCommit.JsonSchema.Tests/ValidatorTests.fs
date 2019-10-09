module ValidatorTests

open System
open Hedgehog
open Swensen.Unquote
open Xunit
open NthCommit.JsonSchema
open NthCommit.JsonSchema.Dom
open Newtonsoft.Json.Linq

module Gen =

    module private Range =

        let decrement range =
            range
            |> Range.map (fun i -> i - 1)

    module Schema =

        let jsonNull = Gen.constant JsonSchemaDocument.Null

        let jsonNumber = Gen.constant JsonSchemaDocument.Number

        let jsonString range =
            Gen.choice [
                Gen.Strings.defaultString |> Gen.list range |> Gen.map JsonString.Enum
                Gen.Strings.defaultString |> Gen.map JsonString.Const 
                Gen.constant JsonString.Unvalidated ]
            |> Gen.map JsonSchemaDocument.String

        let jsonObjectInlineProperty jsonSchemaDocument = gen {
            let! propertyName = Gen.Strings.camelCaseWord
            let! propertyValue = jsonSchemaDocument
            return JsonObjectProperty.Inline (propertyName, propertyValue) }

        let jsonObjectProperty jsonSchemaDocument =
            Gen.choice [
                jsonObjectInlineProperty jsonSchemaDocument ]

        let jsonObject jsonDocument degree depth = gen {
            let nextDepth = Range.decrement depth
            let jsonDocument' = jsonDocument degree nextDepth

            let! properties = jsonObjectProperty jsonDocument' |> Gen.list degree
    
            return JsonSchemaDocument.Object {
                Properties = properties
                PatternProperties = []
                Required = []
                AdditionalProperties = true } }

        let rec jsonDocument degree depth : Gen<JsonSchemaDocument> = Gen.choice [
            jsonNull
            jsonNumber
            jsonString (Range.linear 1 20)
            jsonObject jsonDocument degree depth ]

    module Instance =

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

        let private jsonString (spec : JsonString) =
            match spec with
            | JsonString.Const str -> Gen.constant str
            | JsonString.Enum values -> Gen.item values
            | JsonString.Unvalidated -> Gen.Strings.defaultString
            |> Gen.map createJValue

        let private jsonProperty (json : JsonSchemaDocument -> Gen<JToken>) (spec : JsonObjectProperty) : Gen<JProperty> = gen {
            match spec with
            | Inline (propertyName, schema) ->
                let! propertyValue = json schema
                return JProperty(propertyName, propertyValue)
            | _ ->
                raise (Exception("unhandled"))
                return JProperty(null) }

        let private jsonObject json (spec : JsonObject) : Gen<JToken> = gen {
            let! propertySpecs =
                spec.Properties |> manyOrNoItems
            let! properties =
                propertySpecs
                |> List.map (jsonProperty json)
                |> concat
            return JObject(properties) :> JToken }

        let rec json (schema : JsonSchemaDocument) : Gen<JToken> =
            match schema with
            | JsonSchemaDocument.Null -> createJValue null |> Gen.constant
            | JsonSchemaDocument.Number -> jsonNumber
            | JsonSchemaDocument.String spec -> jsonString spec
            | JsonSchemaDocument.Object spec -> jsonObject json spec
            | _ -> raise (Exception ("Unhandled"))

[<Fact>]
let ``test can generate a valid json instance`` () =
    Property.check <| property {
        let! schema = Gen.Schema.jsonDocument (Range.linear 0 10) (Range.linear 1 5)
        let! instance = Gen.Instance.json schema
        test <@ Validator.validate schema instance |> List.isEmpty @> }
