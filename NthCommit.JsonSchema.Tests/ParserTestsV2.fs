module ParserTestsv2

open Hedgehog
open Xunit
open Swensen.Unquote
open NthCommit.JsonSchema.V2
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System

let jsonPrimitiveNames = ["null"; "string"; "number"; "boolean"; "array"; "object"]

module JToken =

    let primitive (jTokenType : JTokenType) =
        match jTokenType with
        | JTokenType.Object -> JsonPrimitive.Object
        | JTokenType.String -> JsonPrimitive.String
        | JTokenType.Boolean -> JsonPrimitive.Boolean
        | _ -> JsonPrimitive.Unknown

    let serialize (jToken : JToken) = JsonConvert.SerializeObject jToken

[<Fact>]
let ``parses rudimentary schema`` () =
    Property.check <| property {
        let! whitespace = Gen.Strings.whitespace
        let schema = sprintf "{%s}" whitespace
        Ok JsonSchema.Unvalidated =! parse schema }

[<Fact>]
let ``parses schema of given "type"`` () =
    Property.check <| property {
        let! jsonPrimitive = Gen.item jsonPrimitiveNames
        let jsonPrimitive = "string"
        let schema = sprintf @"{ ""type"": ""%s"" }" jsonPrimitive
        Ok <| JsonSchema.String JsonStringSchema.Unvalidated =! parse schema }

module Validation =

    let makeParserError (parserError : ParserError) : Result<JsonSchema, ParserError> =
        Error parserError
    
    let makeSchemaError schemaError =
        schemaError
        |> ParserError.Schema
        |> makeParserError
    
    let makeSchemaTypeError path expectedTypes actualType =
        { Path = path; ExpectedTypes = Set(expectedTypes); ActualType = actualType }
        |> SchemaError.Type
        |> makeSchemaError
    
    let makeSchemaValueError path value =
        { Path = path; Value = value }
        |> SchemaError.Value
        |> makeSchemaError
    
    let invalidJson =
        ParserError.Json
        |> makeParserError

    let expectSchemaTypeError path expectedTypes (actualToken : JToken) schema =
        let tokenType = actualToken.Type
        test <@ makeSchemaTypeError path expectedTypes (tokenType |> JToken.primitive) = parse schema @>

    [<Fact>]
    let ``reports invalid json`` () =
        Property.check <| property {
            let! schema = Gen.Json.invalid
            invalidJson =! parse schema }

    [<Fact>]
    let ``reports schema error when schema is not a json object`` () =
        Property.check <| property {
            let! schemaToken = Gen.Json.tokenNotOf JTokenType.Object
            let schema = JToken.serialize schemaToken
            expectSchemaTypeError "" [JsonPrimitive.Object] schemaToken schema }

    [<Fact>]
    let ``reports schema type error when type is not a string`` () =
        Property.check <| property {
            let! typeToken = Gen.Json.tokenNotOf JTokenType.String
            let schema = sprintf @"{ ""type"": %s }" (JToken.serialize typeToken)
            expectSchemaTypeError "type" [JsonPrimitive.String] typeToken schema }

    [<Fact>]
    let ``reports schema value error when type is not the name of a json primitive`` () =
        Property.check <| property {
            let! typeValue =
                Gen.Strings.defaultString
                |> Gen.notIn jsonPrimitiveNames
            let schema = sprintf @"{ ""type"": ""%s"" }" typeValue
            makeSchemaValueError "type" typeValue =! parse schema }


// TODO: Extended validations for schema
//[<Fact>]
//let ``reports invalid property name`` () =
//    Property.check <| property {
//        let! propertyName = Gen.Strings.camelCaseWord
//        let! propertyValue = Gen.Json.token |> Gen.map JsonConvert.SerializeObject
//        let schema = sprintf @"{ ""%s"": %s }" propertyName propertyValue
//        makeInvalidPropertyName propertyName "" =! parse schema }

//module SchemaType =

//    let validSchemaTypes = ["null"; "string"; "number"; "boolean"; "array"; "object"]

//    [<Fact>]
//    let ``parses rudimentary "type" schema`` () =
//        Property.check <| property {
//            let! schemaType = Gen.item validSchemaTypes
//            let expected : Result<JsonSchema, ParserError> =
//                match schemaType with
//                | "null" -> JsonSchema.Null
//                | "string" -> JsonSchema.String
//                | "number" -> JsonSchema.Number
//                | "boolean" -> JsonSchema.Boolean
//                | "array" -> JsonSchema.Array JsonSchema.Unvalidated
//                | "object" -> JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true; }
//                | _ -> raise (Exception (sprintf "Type '%s' is unhandled by test" schemaType))
//                |> Ok
//            expected =! (parse (sprintf @"{ ""type"": ""%s"" }" schemaType)) }

//    [<Fact>]
//    let ``reports invalid type for "type"`` () =
//        Property.check <| property {
//            let! propertyValue =
//                Gen.Json.tokenNotOf JTokenType.String
//                |> Gen.Json.serialize
//            let schema = sprintf @"{ ""type"": %s }" propertyValue
//            makeInvalidPropertyType "type" ["string"] =! parse schema }
    
//    [<Fact>]
//    let ``reports invalid value for "type"`` () =
//        Property.check <| property {
//            let! propertyValue =
//                Gen.Json.stringValueNotOf validSchemaTypes
//                |> Gen.Json.serialize
//            let schema = sprintf @"{ ""type"": %s }" propertyValue
//            makeInvalidPropertyValue "type" =! parse schema }

//module Properties =

//    [<Fact>]
//    let ``parses trivial nested schema`` () =
//        let schema = @"
//            {
//                ""type"": ""object"",
//                ""properties"": {}
//            }"
//        let expected = JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true } |> Ok
//        expected =! parse schema

//    [<Fact>]
//    let ``parses second trivial nested schema`` () =
//        Property.check <| property {
//            let! propertyName = Gen.Strings.camelCaseWord
//            let! propertyType = Gen.item SchemaType.validSchemaTypes
//            let schema = @"
//                {
//                    ""type"": ""object"",
//                    ""properties"": {
//                        """ + propertyName + @""": {
//                            ""type"": """ + propertyType + @"""
//                        }
//                    }
//                }"
//            let expectedType =
//                match propertyType with
//                | "null" -> JsonSchema.Null
//                | "string" -> JsonSchema.String
//                | "number" -> JsonSchema.Number
//                | "boolean" -> JsonSchema.Boolean
//                | "array" -> JsonSchema.Array JsonSchema.Unvalidated
//                | "object" -> JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true; }
//                | _ -> raise (Exception (sprintf "Type '%s' is unhandled by test" propertyType))
//            let expected =
//                JsonSchema.Object {
//                    Properties = [
//                        (propertyName, expectedType)]
//                    Required = []
//                    AdditionalProperties = true }
//                |> Ok
//            expected =! parse schema }

//    [<Fact>]
//    let ``reports "properties" is invalid if json type is not "object"`` () =
//        Property.check <| property {
//            let! schemaType =
//                SchemaType.validSchemaTypes
//                |> List.filter ((<>) "object")
//                |> Gen.item
//            let! propertiesValue =
//                Gen.Json.token
//                |> Gen.Json.serialize
//            let schema = sprintf @"{ ""type"": ""%s"", ""properties"": %s }" schemaType propertiesValue
//            let expected =
//                makeInvalidPropertyName
//                    "properties"
//                    "Property 'properties' is only valid when 'type' is 'object'"
//            expected =! parse schema }

//    [<Fact>]
//    let ``reports invalid type for "properties"`` () =
//        Property.check <| property {
//            let! propertiesValue =
//                Gen.Json.tokenNotOf JTokenType.Object
//                |> Gen.Json.serialize
//            let schema = sprintf @"{ ""type"": ""object"", ""properties"": %s }" propertiesValue
//            makeInvalidPropertyType "properties" ["object"] =! parse schema }

//    [<Fact>]
//    let ``reports invalid value for "properties" if not an object of objects`` () =
//        Property.check <| property {
//            let! propertiesPropertyName =
//                Gen.Strings.camelCaseWord
//            let! propertiesPropertyValue =
//                Gen.Json.tokenNotOf JTokenType.Object
//                |> Gen.Json.serialize
//            let schema =
//                sprintf
//                    @"{ ""type"": ""object"", ""properties"": { ""%s"": %s } }"
//                    propertiesPropertyName
//                    propertiesPropertyValue
//            let expectedPath = sprintf "properties.%s" propertiesPropertyName
//            makeInvalidPropertyType expectedPath ["object"] =! parse schema }
