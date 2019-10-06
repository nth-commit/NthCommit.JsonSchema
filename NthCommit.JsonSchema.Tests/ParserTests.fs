module ParserTests

open Hedgehog
open Xunit
open Swensen.Unquote
open NthCommit.JsonSchema
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System

module private Result =

    let isErrorAnd f =
        function
        | Ok _ -> false
        | Error x -> f x

let parseObj (obj : JObject) =
    JsonConvert.SerializeObject obj |> parse

let parseProperties (props : JProperty list) =
    JObject (props) |> parseObj

let parseProperty (prop : JProperty) =
    [prop] |> parseProperties

let reportsInvalidJson =
    let isInvalidJson =
        function
        | ParserError.InvalidJson -> true
        | _ -> false
    Result.isErrorAnd isInvalidJson

let jsonEquals (a : JToken) (b : JToken) =
    let serialize = JsonConvert.SerializeObject
    serialize a = serialize b

let reportsInvalidPropertyName property =
    Result.isErrorAnd <| function
        | ParserError.InvalidPropertyName p -> jsonEquals p property
        | _ -> false

let reportsInvalidPropertyType property =
    Result.isErrorAnd <| function
        | ParserError.InvalidPropertyType p -> jsonEquals p property
        | _ -> false

let reportsInvalidPropertyValue property =
    Result.isErrorAnd <| function
        | ParserError.InvalidPropertyValue p -> jsonEquals p property
        | _ -> false

let reportsSchemaViolation token trivia =
    Result.isErrorAnd <| function
        | ParserError.SchemaViolation (tkn, trv)  ->
            jsonEquals tkn token && trv = trivia
        | _ -> false

[<Fact>]
let ``parses rudimentary schema`` () =
    Property.check <| property {
        let! whitespace = Gen.Strings.whitespace
        let schema = sprintf "{%s}" whitespace
        test <@ parse schema = Ok JsonSchema.Unvalidated @> }

[<Fact>]
let ``reports invalid json`` () =
    Property.check <| property {
        let! schema = Gen.Json.invalid
        test <@ parse schema |> reportsInvalidJson @> }

[<Fact>]
let ``reports invalid property name`` () =
    Property.check <| property {
        let! property = Gen.Json.property Gen.Strings.camelCaseWord
        let schemaObj = JObject ([property])
        test <@ parseObj schemaObj |> reportsInvalidPropertyName property @> }

module JsonType =

    let validJsonTypes = ["null"; "string"; "number"; "boolean"; "array"; "object"]

    [<Fact>]
    let ``reports invalid type for "type"`` () =
        Property.check <| property {
            let! property =
                Gen.Json.propertyWithValue
                    (Gen.constant "type")
                    (Gen.Json.tokenNotOf JTokenType.String)
            test <@ parseProperty property |> reportsInvalidPropertyType property @> }
    
    [<Fact>]
    let ``reports invalid value for "type"`` () =
        Property.check <| property {
            let! property =
                Gen.Json.propertyWithValue
                    (Gen.constant "type")
                    (Gen.Json.stringValueNotOf validJsonTypes)
            test <@ parseProperty property |> reportsInvalidPropertyValue property @> }

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
            test <@ expected = (parse (sprintf @"{ ""type"": ""%s"" }" jsonType)) @> }

module Gen =

    let typeProperty (values : string list) =
        let typeValue =
            Gen.item values
            |> Gen.map (fun x -> JValue(x) :> JToken)
        Gen.Json.propertyWithValue
            (Gen.constant "type")
            typeValue

    let propertiesProperty = Gen.Json.property (Gen.constant "properties")

module Properties =

    [<Fact>]
    let ``reports "properties" is invalid if json type is not "object"`` () =
        Property.check <| property {
            let! typeProperty =
                JsonType.validJsonTypes
                |> List.filter ((<>) "object")
                |> Gen.typeProperty
            let! propertiesProperty = Gen.propertiesProperty
            test <@ parseProperties [typeProperty; propertiesProperty]
                    |> reportsSchemaViolation propertiesProperty "Property 'properties' is only valid when 'type' is 'object'" @> }

    [<Fact>]
    let ``reports invalid type for "properties"`` () =
        Property.check <| property {
            let! typeProperty = ["object"] |> Gen.typeProperty
            let! propertiesProperty =
                Gen.Json.propertyWithValue
                    (Gen.constant "properties")
                    (Gen.Json.tokenNotOf JTokenType.Object)
            test <@ parseProperties [typeProperty; propertiesProperty]
                    |> reportsInvalidPropertyType propertiesProperty @> }