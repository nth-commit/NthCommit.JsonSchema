namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq
open JsonHelper

module private Map =

    let keys map =
        map
        |> Map.toList
        |> List.map fst
        |> Set

module private List =

    let toMap keyProjection list =
        list
        |> List.map (fun x -> (keyProjection x, x))
        |> Map

module private Result =

    let append (f : 'a -> Result<'b, 'TError>) (x : Result<'a, 'TError>) : Result<('a * 'b), 'TError> =
        x |> Result.bind (fun a -> f a |> Result.map (fun b -> (a, b)))

[<AutoOpen>]
module Parser =

    type SchemaViolationTrivia = string

    [<RequireQualifiedAccess>]
    type ParserError =
        | InvalidJson
        | InvalidPropertyName of JProperty
        | InvalidPropertyType of JProperty
        | InvalidPropertyValue of JProperty
        | SchemaViolation of JToken * SchemaViolationTrivia
        | Unhandled

    module JsonTypeParser =

        [<RequireQualifiedAccess>]
        type JsonType =
            | Null
            | String
            | Number
            | Boolean
            | Object
            | Array
            | Unset

        let private matchJsonType p = function
            | "null" -> JsonType.Null |> Ok
            | "string" -> JsonType.String |> Ok
            | "number" -> JsonType.Number |> Ok
            | "boolean" -> JsonType.Boolean |> Ok
            | "array" -> JsonType.Array |> Ok
            | "object" -> JsonType.Object |> Ok
            | _ -> ParserError.InvalidPropertyValue p |> Error

        let parse (propertyOpt : JProperty option) : Result<JsonType, ParserError> =
            match propertyOpt with
            | Some property ->
                match matchJToken property.Value with
                | JsonString s -> matchJsonType property s
                | _ -> ParserError.InvalidPropertyType property |> Error
            | None _ -> JsonType.Unset |> Ok

    module PropertiesParser =

        type Properties =
            | Unset

        let parse (propertyOpt : JProperty option) (jsonType : JsonTypeParser.JsonType) : Result<Properties, ParserError> =
            match propertyOpt with
            | Some property ->
                match jsonType with
                | JsonTypeParser.JsonType.Object _ ->
                    match matchJToken property.Value with
                    | _ -> ParserError.InvalidPropertyType property |> Error
                | _ ->
                    let trivia = "Property 'properties' is only valid when 'type' is 'object'"
                    ParserError.SchemaViolation (property, trivia) |> Error
            | None -> Properties.Unset |> Ok

    let private keyPropertiesByName (properties : JProperty list) : Map<string, JProperty> =
        properties
        |> List.toMap (fun p -> p.Name)

    let private tryFindPropertyWithInvalidName (properties : JProperty list) : JProperty option =
        let propertiesByName = keyPropertiesByName properties
        Set.difference (propertiesByName |> Map.keys) (Set(["type"; "properties"]))
        |> Seq.map (fun n -> Map.find n propertiesByName)
        |> Seq.tryHead

    let private parseSchemaProperties (properties : JProperty list) : Result<JsonSchema, ParserError> =
        match tryFindPropertyWithInvalidName properties with
        | Some property -> ParserError.InvalidPropertyName property |> Error 
        | None ->
            let propertiesByName = keyPropertiesByName properties
            propertiesByName |> Map.tryFind "type" |> JsonTypeParser.parse
            |> Result.append (propertiesByName |> Map.tryFind "properties" |> PropertiesParser.parse)
            |> Result.map (fun (jsonType, _) ->
                match jsonType with
                | JsonTypeParser.JsonType.Null -> JsonSchema.Null
                | JsonTypeParser.JsonType.String -> JsonSchema.String
                | JsonTypeParser.JsonType.Number -> JsonSchema.Number
                | JsonTypeParser.JsonType.Boolean -> JsonSchema.Boolean
                | JsonTypeParser.JsonType.Array -> JsonSchema.Array (JsonSchema.Unvalidated)
                | JsonTypeParser.JsonType.Object -> JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true }
                | JsonTypeParser.JsonType.Unset -> JsonSchema.Unvalidated)

    let private parseSchemaToken (token : JsonToken) : Result<JsonSchema, ParserError> =
        match token with
        | JsonObject properties -> parseSchemaProperties properties
        | _ -> raise (Exception "")

    let private parseSchemaElement schemaToken : Result<JsonSchema, ParserError> =
        matchJToken schemaToken
        |> parseSchemaToken

    let private deserialize schema =
        tryDeserialize schema
        |> Result.mapError (fun _ -> ParserError.InvalidJson)

    let parse (schema : string) : Result<JsonSchema, ParserError> =
        deserialize schema
        |> Result.bind parseSchemaElement