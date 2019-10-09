module ValidationTests

open Xunit
open Hedgehog
open NthCommit.JsonSchema
open Swensen.Unquote

[<Fact>]
let ``invalid schema json returns parser error of invalid json`` () =
    Property.check <| property {
        let! schema = Gen.Json.invalid
        let! instance = Gen.Strings.defaultString
        test <@ JsonSchema.validate schema instance
                |> List.exactlyOne
                |> (fun x ->
                        x.Target = ValidationFailureTarget.Schema &&
                        x.Type  = ValidationFailureType.InvalidJson) @> }

[<Fact>]
let ``invalid instance json returns schema error of invalid json`` () =
    Property.check <| property {
        let! instance = Gen.Json.invalid
        test <@ JsonSchema.validate "{}" instance
                |> List.exactlyOne
                |> (fun x ->
                        x.Target = ValidationFailureTarget.Instance &&
                        x.Type  = ValidationFailureType.InvalidJson) @> }

[<Fact>]
let ``any instance is valid against rudimentary schema`` () =
    Property.check <| property {
        let! instance = Gen.Json.valid
        test <@ [] = JsonSchema.validate "{}" instance @> }

[<Fact>]
let ``any instance is valid against schema of the same json primitive`` () =
    Property.check <| property {
        let! primitive = Gen.Json.primitive
        let! instance = Gen.Json.jsonOfPrimitive primitive
        let schema = sprintf @"{ ""type"": ""%s"" }" <| primitive.ToSchemaTypeValue()
        test <@ [] = JsonSchema.validate schema instance @> }

[<Fact>]
let ``any instance is invalid against schema of a different json primitive`` () =
    Property.check <| property {
        let! schemaPrimitive = Gen.Json.primitive
        let! instancePrimitive = Gen.Json.primitive |> Gen.filter ((<>) schemaPrimitive)
        let! instance = Gen.Json.jsonOfPrimitive instancePrimitive
        let schema = sprintf @"{ ""type"": ""%s"" }" <| schemaPrimitive.ToSchemaTypeValue()
        let expectedMessage =
            sprintf
                @"Error validating instance: expected type of %s but recieved %s at path ""#"""
                (schemaPrimitive.ToString())
                (instancePrimitive.ToString())
        test <@ JsonSchema.validate schema instance
                |> List.exactlyOne
                |> (fun x ->
                        x.Target = ValidationFailureTarget.Instance &&
                        x.Type  = ValidationFailureType.SchemaViolation &&
                        x.Message = expectedMessage) @> }
