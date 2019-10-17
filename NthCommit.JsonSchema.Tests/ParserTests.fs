module ParserTests

open System
open Hedgehog
open Xunit
open Swensen.Unquote
open NthCommit.JsonSchema
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.Parsing
open NthCommit.JsonSchema.Validation
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let raiseArrogantException () =
    raise (Exception ("No way will this fail"))

let parseArrogantly schemaJson =
    match parse schemaJson with
    | Ok schema -> schema
    | Error _ -> raiseArrogantException ()

let parseObjectArrogantly schemaJson =
    match parseArrogantly schemaJson with
    | JsonElementSchema.Object objectSchema -> objectSchema
    | _ -> raiseArrogantException ()

let primitiveNames = ["null"; "string"; "number"; "boolean"; "array"; "object"]

let trivialNullSchema = JsonElementSchema.Null

let trivialBooleanSchema = JsonElementSchema.Boolean

let trivialNumberSchema = JsonElementSchema.Number

let trivialStringSchema =
    JsonElementSchema.String JsonStringSchema.Unvalidated

let trivialArraySchema =
    JsonElementSchema.Array { Items = JsonElementSchema.Unvalidated }

let defaultObjectSchema =
    JsonElementSchema.Object {
        Properties              = []
        PatternProperties       = []
        Required                = Set []
        AdditionalProperties    = true }

module JToken =

    let primitive (jToken : JToken) =
        match jToken.Type with
        | JTokenType.Null       -> JsonPrimitive.Null
        | JTokenType.Boolean    -> JsonPrimitive.Boolean
        | JTokenType.Integer    -> JsonPrimitive.Number
        | JTokenType.String     -> JsonPrimitive.String
        | JTokenType.Array      -> JsonPrimitive.Array
        | JTokenType.Object     -> JsonPrimitive.Object
        | _                     -> raise (Exception ("Unhandled primitive"))

    let serialize (jToken : JToken) = JsonConvert.SerializeObject jToken

[<Fact>]
let ``parses rudimentary schema`` () =
    Property.check <| property {
        let! whitespace = Gen.Strings.whitespace
        let schema      = sprintf "{%s}" whitespace
        Ok JsonElementSchema.Unvalidated =! parse schema }

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
        | "object"  -> <@ defaultObjectSchema   |> Ok = parse schema @>
        | x         -> <@ x <> x @> // We must have an unhandled primitive
        |> test }

let makeParserError (parserError : ParserError) : Result<JsonElementSchema, ParserError> =
    Error parserError

let makeSchemaError schemaError =
    schemaError
    |> ParserError.Schema
    |> makeParserError

let makeSchemaTypeError path expectedTypes actualType =
    { Path = path; ExpectedTypes = Set(expectedTypes); ActualType = actualType }
    |> SchemaError.Type
    |> makeSchemaError

let expectSchemaTypeError path expectedTypes actualType schema =
    test <@ makeSchemaTypeError path expectedTypes actualType = parse schema @>

module Validation =

    let makeSchemaValueError path value =
        { Path = path; Value = value }
        |> SchemaError.Value
        |> makeSchemaError

    let invalidJson =
        SchemaError.Json ""
        |> ParserError.Schema
        |> makeParserError

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
            expectSchemaTypeError "#" [JsonPrimitive.Object] (JToken.primitive schemaToken) schema }

    [<Fact>]
    let ``reports schema type error when type is not a string`` () =
        Property.check <| property {
            let! typeToken = Gen.Json.tokenNotOf JTokenType.String
            let schema = sprintf @"{ ""type"": %s }" (JToken.serialize typeToken)
            expectSchemaTypeError "type" [JsonPrimitive.String] (JToken.primitive typeToken) schema }

    [<Fact>]
    let ``reports schema value error when type is not the name of a json primitive`` () =
        Property.check <| property {
            let! typeValue =
                Gen.Strings.defaultString
                |> Gen.notIn primitiveNames
            let schema = sprintf @"{ ""type"": ""%s"" }" typeValue
            makeSchemaValueError "type" typeValue =! parse schema }

module Object =

    module Gen =

        let rudimentarySchema =
            primitiveNames
            |> Gen.item
            |> Gen.map (sprintf @"{ ""type"": ""%s"" }")

        let x = 0

    [<Fact>]
    let ``rudimentary object schema returns correct defaults`` () =
        let schema = @"{ ""type"": ""object"" }"
        test <@ defaultObjectSchema = parseArrogantly schema @>

    [<Fact>]
    let ``parses object schema with properties set`` () =
        Property.check <| property {
            let! propertyName = Gen.Strings.camelCaseWord
            let! propertySchema = Gen.rudimentarySchema
            let schema =
                sprintf
                    @"{
                        ""type"": ""object"",
                        ""properties"": {
                            ""%s"": %s
                        }
                    }"
                    propertyName
                    propertySchema
            test <@ (parseObjectArrogantly schema).Properties
                    |> List.exactlyOne
                    |> (fun p -> p.Name = propertyName) @> }

    module Validation =

        [<Fact>]
        let ``reports schema type error when properties is not a json object`` () =
            Property.check <| property {
                let! propertiesToken = Gen.Json.tokenNotOf JTokenType.Object
                let schema = sprintf @"{ ""type"": ""object"", ""properties"": %s }" (JToken.serialize propertiesToken)
                let expectedError = makeSchemaTypeError "properties" [JsonPrimitive.Object] (JToken.primitive propertiesToken)
                test <@ expectedError = parse schema @> }

        [<Fact>]
        let ``reports schema type error when properties/foo is not a json object`` () =
            Property.check <| property {
                let schema = @"{ ""type"": ""object"", ""properties"": { ""foo"": ""bar"" }"
                let expectedError = makeSchemaTypeError "properties.foo" [JsonPrimitive.Object] JsonPrimitive.String
                test <@ expectedError = parse schema @> }

        [<Fact>]
        let ``reports schema type error when properties/foo/type is not a string`` () =
            Property.check <| property {
                let schema = @"{ ""type"": ""object"", ""properties"": { ""foo"": { ""type"": true } }"
                let expectedError = makeSchemaTypeError "properties.foo.type" [JsonPrimitive.String] JsonPrimitive.Boolean
                test <@ expectedError = parse schema @> }

// TODO: Report existing members that are not in the set of what we capture, as the standard is not fully supported so
//       should flag up these. Will need to add a strict mode (false)
// TODO: Nested properties validation