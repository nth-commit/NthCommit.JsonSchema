module ParserTests

open Hedgehog
open Xunit
open Swensen.Unquote
open NthCommit.JsonSchema
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System

let makeParserError (parserError : ParserError) : Result<JsonSchema, ParserError> =
    Error parserError

let invalidJson =
    ParserError.InvalidJson
    |> makeParserError

let makeInvalidPropertyType path =
    ParserError.InvalidPropertyType path
    |> makeParserError

let makeInvalidPropertyName path trivia =
    ParserError.InvalidPropertyName (path, trivia)
    |> makeParserError

let makeInvalidPropertyValue path =
    ParserError.InvalidPropertyValue path
    |> makeParserError

[<Fact>]
let ``parses rudimentary schema`` () =
    Property.check <| property {
        let! whitespace = Gen.Strings.whitespace
        let schema = sprintf "{%s}" whitespace
        parse schema =! Ok JsonSchema.Unvalidated }

[<Fact>]
let ``reports invalid json`` () =
    Property.check <| property {
        let! schema = Gen.Json.invalid
        invalidJson =! parse schema }

[<Fact>]
let ``reports invalid property name`` () =
    Property.check <| property {
        let! propertyName = Gen.Strings.camelCaseWord
        let! propertyValue = Gen.Json.token |> Gen.map JsonConvert.SerializeObject
        let schema = sprintf @"{ ""%s"": %s }" propertyName propertyValue
        makeInvalidPropertyName propertyName "" =! parse schema }

module JsonType =

    let validJsonTypes = ["null"; "string"; "number"; "boolean"; "array"; "object"]

    [<Fact>]
    let ``reports invalid type for "type"`` () =
        Property.check <| property {
            let! propertyValue =
                Gen.Json.tokenNotOf JTokenType.String
                |> Gen.Json.serialize
            let schema = sprintf @"{ ""type"": %s }" propertyValue
            makeInvalidPropertyType "type" =! parse schema }
    
    [<Fact>]
    let ``reports invalid value for "type"`` () =
        Property.check <| property {
            let! propertyValue =
                Gen.Json.stringValueNotOf validJsonTypes
                |> Gen.Json.serialize
            let schema = sprintf @"{ ""type"": %s }" propertyValue
            makeInvalidPropertyValue "type" =! parse schema }

    [<Fact>]
    let ``parses rudimentary typed schema`` () =
        Property.check <| property {
            let! jsonType = Gen.item validJsonTypes
            let expected : Result<JsonSchema, ParserError> =
                match jsonType with
                | "null" -> JsonSchema.Null
                | "string" -> JsonSchema.String
                | "number" -> JsonSchema.Number
                | "boolean" -> JsonSchema.Boolean
                | "array" -> JsonSchema.Array JsonSchema.Unvalidated
                | "object" -> JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true; }
                | _ -> raise (Exception (sprintf "Type '%s' is unhandled by test" jsonType))
                |> Ok
            expected =! (parse (sprintf @"{ ""type"": ""%s"" }" jsonType)) }

module Properties =

    [<Fact>]
    let ``reports "properties" is invalid if json type is not "object"`` () =
        Property.check <| property {
            let! jsonType =
                JsonType.validJsonTypes
                |> List.filter ((<>) "object")
                |> Gen.item
            let! propertiesValue =
                Gen.Json.token
                |> Gen.Json.serialize
            let schema = sprintf @"{ ""type"": ""%s"", ""properties"": %s }" jsonType propertiesValue
            let expected =
                makeInvalidPropertyName
                    "properties"
                    "Property 'properties' is only valid when 'type' is 'object'"
            expected =! parse schema }

    [<Fact>]
    let ``reports invalid type for "properties"`` () =
        Property.check <| property {
            let! propertiesValue =
                Gen.Json.tokenNotOf JTokenType.Object
                |> Gen.Json.serialize
            let schema = sprintf @"{ ""type"": ""object"", ""properties"": %s }" propertiesValue
            makeInvalidPropertyType "properties" =! parse schema }

    [<Fact>]
    let ``reports invalid value for "properties" if not an object of objects`` () =
        Property.check <| property {
            let! propertiesPropertyName =
                Gen.Strings.camelCaseWord
            let! propertiesPropertyValue =
                Gen.Json.tokenNotOf JTokenType.Object
                |> Gen.Json.serialize
            let schema =
                sprintf
                    @"{ ""type"": ""object"", ""properties"": { ""%s"": %s } }"
                    propertiesPropertyName
                    propertiesPropertyValue
            let expectedPath = sprintf "properties.%s" propertiesPropertyName
            makeInvalidPropertyValue expectedPath =! parse schema }