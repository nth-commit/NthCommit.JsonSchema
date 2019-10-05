module ParserTests

open Hedgehog
open Xunit
open Swensen.Unquote
open NthCommit.JsonSchema
open Newtonsoft.Json
open Newtonsoft.Json.Linq

module private Result =

    let isOk =
        function
        | Ok _ -> true
        | _ -> false

    let isErrorAnd f =
        function
        | Ok _ -> false
        | Error x -> f x

let parseObj (obj : JObject) =
    JsonConvert.SerializeObject obj
    |> parse

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

[<Fact>]
let ``parsing rudimentary schema`` () =
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

[<Fact>]
let ``reports invalid property type`` () =
    Property.check <| property {
        let name = Gen.constant "type"
        let value = Gen.Json.tokenNotOf JTokenType.String
        let! property = Gen.Json.propertyWithValue name value
        let schemaObj = JObject ([property])
        test <@ parseObj schemaObj |> reportsInvalidPropertyType property @> }

[<Fact>]
let ``reports invalid property value`` () =
    Property.check <| property {
        let name = Gen.constant "type"
        let value = Gen.Json.tokenOf JTokenType.String
        let! property = Gen.Json.propertyWithValue name value
        let schemaObj = JObject ([property])
        test <@ parseObj schemaObj |> reportsInvalidPropertyValue property @> }

[<Fact>]
let ``parses valid json types`` () =
    Property.check <| property {
        let name = Gen.constant "type"
        let value = Gen.constant (JValue ("object") :> JToken)
        let! property = Gen.Json.propertyWithValue name value
        let schemaObj = JObject ([property])
        test <@ parseObj schemaObj = Ok (JsonSchema.Object { Properties = []; RequiredProperies = []; AdditionalProperties = true; })@> }