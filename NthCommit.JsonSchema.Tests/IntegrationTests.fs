module IntegrationTests

open Xunit
open Hedgehog
open Swensen.Unquote
open NthCommit.JsonSchema

let getValidationErrors (schema, instance) = JsonSchema.validate schema instance |> Seq.toList

let getExactlyOneValidationError = getValidationErrors >> List.exactlyOne

[<Fact>]
let ``test instance is valid against schema`` () =
    Property.check <| property {
        let! schema = Gen.Schema.Defaults.jsonElement
        let! instanceJson = ValidatorTests.Gen.Instance.json schema
        test <@ [] = getValidationErrors (JsonElementSchema.serialize schema, instanceJson) @> }

[<Fact>]
let ``test invalid schema json`` () =
    let schema = "I'm some invalid json"
    let instance = "{}"
    {   Target = ValidationFailureTarget.Schema
        Type = ValidationFailureType.InvalidJson
        Path = ""
        Message = "Invalid schema: JSON was invalid. Path '', line 1, position 1" } =! getExactlyOneValidationError (schema, instance)

[<Fact>]
let ``test invalid instance json`` () =
    let schema = "{}"
    let instance = "I'm some invalid json"
    {   Target = ValidationFailureTarget.Instance
        Type = ValidationFailureType.InvalidJson
        Path = ""
        Message = "Invalid instance: JSON was invalid. Path '', line 1, position 1" } =! getExactlyOneValidationError (schema, instance)

[<Fact>]
let ``test type error`` () =
    let schema = @"{ ""type"": ""object"" }"
    let instance = "[]"
    {   Target = ValidationFailureTarget.Instance
        Type = ValidationFailureType.SchemaViolation
        Path = ""
        Message = "Invalid instance: Expected type of ['object'], but found 'array'" } =! getExactlyOneValidationError (schema, instance)

[<Fact>]
let ``test value error`` () =
    let schema =
        @"{
            ""type"": ""object"",
            ""properties"": {
                ""foo"": {
                    ""type"": ""string"",
                    ""const"": ""bar""
                }
            }
        }"
    let instance = @"{ ""foo"": ""baz"" }"
    {   Target = ValidationFailureTarget.Instance
        Type = ValidationFailureType.SchemaViolation
        Path = "foo"
        Message = "Invalid instance: Expected value to be in ['bar'], but found 'baz'" } =! getExactlyOneValidationError (schema, instance)

[<Fact>]
let ``test required properties error`` () =
    let schema =
        @"{
            ""type"": ""object"",
            ""required"": [""foo""]
        }"
    let instance = @"{ ""fluff"": 1 }"
    {   Target = ValidationFailureTarget.Instance
        Type = ValidationFailureType.SchemaViolation
        Path = ""
        Message = "Invalid instance: Property 'foo' was required" } =! getExactlyOneValidationError (schema, instance)

[<Fact>]
let ``test additional properties error`` () =
    let schema =
        @"{
            ""type"": ""object"",
            ""additionalProperties"": false
        }"
    let instance = @"{ ""foo"": 1 }"
    {   Target = ValidationFailureTarget.Instance
        Type = ValidationFailureType.SchemaViolation
        Path = ""
        Message = "Invalid instance: Additional property 'foo' was detected" } =! getExactlyOneValidationError (schema, instance)