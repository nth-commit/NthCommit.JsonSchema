module ParserTests

open Hedgehog
open Xunit
open Swensen.Unquote
open NthCommit.JsonSchema
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System
open NthCommit.JsonSchema.Validator

let primitiveNames = ["null"; "string"; "number"; "boolean"; "array"; "object"]

let trivialNullSchema = JsonSchema.Null

let trivialBooleanSchema = JsonSchema.Boolean

let trivialNumberSchema = JsonSchema.Number

let trivialStringSchema =
    JsonSchema.String JsonStringSchema.Unvalidated

let trivialArraySchema =
    JsonSchema.Array { Items = JsonSchema.Unvalidated }

let trivialObjectSchema =
    JsonSchema.Object {
        Properties              = []
        PatternProperties       = []
        Required                = []
        AdditionalProperties    = true }

module JToken =

    let primitive (jTokenType : JTokenType) =
        match jTokenType with
        | JTokenType.Object     -> JsonPrimitive.Object
        | JTokenType.String     -> JsonPrimitive.String
        | JTokenType.Boolean    -> JsonPrimitive.Boolean
        | _ -> JsonPrimitive.Unknown

    let serialize (jToken : JToken) = JsonConvert.SerializeObject jToken

[<Fact>]
let ``parses rudimentary schema`` () =
    Property.check <| property {
        let! whitespace = Gen.Strings.whitespace
        let schema      = sprintf "{%s}" whitespace
        Ok JsonSchema.Unvalidated =! parse schema }

[<Fact>]
let ``parses schema of given "type"`` () =
    Property.check <| property {
        let! primitive = Gen.item primitiveNames
        let! schema =
            sprintf @"{ ""type"": ""%s"" }" primitive
            |> Gen.Json.maybeAdditive
        match primitive with
        | "null"    -> <@ trivialNullSchema     |> Ok = parse schema @>
        | "boolean" -> <@ trivialBooleanSchema  |> Ok = parse schema @>
        | "number"  -> <@ trivialNumberSchema   |> Ok = parse schema @>
        | "string"  -> <@ trivialStringSchema   |> Ok = parse schema @>
        | "array"   -> <@ trivialArraySchema    |> Ok = parse schema @>
        | "object"  -> <@ trivialObjectSchema   |> Ok = parse schema @>
        | x         -> <@ x <> x @> // We must have an unhandled primitive
        |> test }

module Object = 

    [<Fact>]
    let ``parses rudimentary object schema with properties`` () =
        Property.check <| property {
            let! schema =
                @"{
                    ""type"": ""object"",
                    ""properties"": {}
                }"
                |> Gen.Json.maybeAdditive
            test <@ Ok <| trivialObjectSchema = parse schema @> }

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
                |> Gen.notIn primitiveNames
            let schema = sprintf @"{ ""type"": ""%s"" }" typeValue
            makeSchemaValueError "type" typeValue =! parse schema }

    [<Fact>]
    let ``reports schema type error when properties is not an object`` () =
        Property.check <| property {
            let! propertiesToken = Gen.Json.tokenNotOf JTokenType.Object
            let schema = sprintf @"{ ""type"": ""object"", ""properties"": %s }" (JToken.serialize propertiesToken)
            expectSchemaTypeError "properties" [JsonPrimitive.Object] propertiesToken schema }

// TODO: Property members validation e.g. has any member that is not an object
// TODO: Report existing members that are not in the set of what we capture, as the standard is not fully supported so
//       should flag up these. Will need to add a strict mode (false)
// TODO: Nested properties validation