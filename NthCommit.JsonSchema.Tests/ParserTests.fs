﻿module ParserTests

open Hedgehog
open Xunit
open Swensen.Unquote
open NthCommit.JsonSchema
open NthCommit.JsonSchema.Parsing
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System

let makeParserError (parserError : ParserError) : Result<JsonSchema, ParserError> =
    Error parserError

let invalidJson =
    ParserError.Json
    |> makeParserError

let makeInvalidPropertyType path acceptedTypes =
    ParserError.PropertyType { Path = path; AcceptedTypes = acceptedTypes }
    |> makeParserError

let makeInvalidPropertyName path trivia =
    ParserError.PropertyName { Path = path; Trivia = trivia }
    |> makeParserError

let makeInvalidPropertyValue path =
    ParserError.PropertyValue path
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

module SchemaType =

    let validSchemaTypes = ["null"; "string"; "number"; "boolean"; "array"; "object"]

    [<Fact>]
    let ``parses rudimentary "type" schema`` () =
        Property.check <| property {
            let! schemaType = Gen.item validSchemaTypes
            let expected : Result<JsonSchema, ParserError> =
                match schemaType with
                | "null" -> JsonSchema.Null
                | "string" -> JsonSchema.String
                | "number" -> JsonSchema.Number
                | "boolean" -> JsonSchema.Boolean
                | "array" -> JsonSchema.Array JsonSchema.Unvalidated
                | "object" -> JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true; }
                | _ -> raise (Exception (sprintf "Type '%s' is unhandled by test" schemaType))
                |> Ok
            expected =! (parse (sprintf @"{ ""type"": ""%s"" }" schemaType)) }

    [<Fact>]
    let ``reports invalid type for "type"`` () =
        Property.check <| property {
            let! propertyValue =
                Gen.Json.tokenNotOf JTokenType.String
                |> Gen.Json.serialize
            let schema = sprintf @"{ ""type"": %s }" propertyValue
            makeInvalidPropertyType "type" ["string"] =! parse schema }
    
    [<Fact>]
    let ``reports invalid value for "type"`` () =
        Property.check <| property {
            let! propertyValue =
                Gen.Json.stringValueNotOf validSchemaTypes
                |> Gen.Json.serialize
            let schema = sprintf @"{ ""type"": %s }" propertyValue
            makeInvalidPropertyValue "type" =! parse schema }

module Properties =

    [<Fact>]
    let ``parses trivial nested schema`` () =
        let schema = @"
            {
                ""type"": ""object"",
                ""properties"": {}
            }"
        let expected = JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true } |> Ok
        expected =! parse schema

    [<Fact>]
    let ``parses second trivial nested schema`` () =
        Property.check <| property {
            let! propertyName = Gen.Strings.camelCaseWord
            let! propertyType = Gen.item SchemaType.validSchemaTypes
            let schema = @"
                {
                    ""type"": ""object"",
                    ""properties"": {
                        """ + propertyName + @""": {
                            ""type"": """ + propertyType + @"""
                        }
                    }
                }"
            let expectedType =
                match propertyType with
                | "null" -> JsonSchema.Null
                | "string" -> JsonSchema.String
                | "number" -> JsonSchema.Number
                | "boolean" -> JsonSchema.Boolean
                | "array" -> JsonSchema.Array JsonSchema.Unvalidated
                | "object" -> JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true; }
                | _ -> raise (Exception (sprintf "Type '%s' is unhandled by test" propertyType))
            let expected =
                JsonSchema.Object {
                    Properties = [
                        (propertyName, expectedType)]
                    Required = []
                    AdditionalProperties = true }
                |> Ok
            expected =! parse schema }

    [<Fact>]
    let ``reports "properties" is invalid if json type is not "object"`` () =
        Property.check <| property {
            let! schemaType =
                SchemaType.validSchemaTypes
                |> List.filter ((<>) "object")
                |> Gen.item
            let! propertiesValue =
                Gen.Json.token
                |> Gen.Json.serialize
            let schema = sprintf @"{ ""type"": ""%s"", ""properties"": %s }" schemaType propertiesValue
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
            makeInvalidPropertyType "properties" ["object"] =! parse schema }

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
            makeInvalidPropertyType expectedPath ["object"] =! parse schema }
