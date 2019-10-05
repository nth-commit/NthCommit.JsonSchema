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

[<AutoOpen>]
module Parser =

    [<RequireQualifiedAccess>]
    type ParserError =
        | InvalidJson
        | InvalidPropertyName of JProperty
        | InvalidPropertyType of JProperty
        | InvalidPropertyValue of JProperty
        | Unhandled

    module JsonTypeParser =

        type JsonType =
            | Object
            | Unset

        let matchJsonType p = function
            | "object" -> JsonType.Object |> Ok
            | _ -> ParserError.InvalidPropertyValue p |> Error

        let parse (propertyOpt : JProperty option) : Result<JsonType, ParserError> =
            match propertyOpt with
            | Some property ->
                match matchJToken property.Value with
                | JsonString s -> matchJsonType property s
                | _ -> ParserError.InvalidPropertyType property |> Error
            | None _ -> Unset |> Ok

    let private keyPropertiesByName (properties : JProperty list) : Map<string, JProperty> =
        properties
        |> List.toMap (fun p -> p.Name)

    let private tryFindPropertyWithInvalidName (properties : JProperty list) : JProperty option =
        let propertiesByName = keyPropertiesByName properties
        Set.difference (propertiesByName |> Map.keys) (Set(["type"]))
        |> Seq.map (fun n -> Map.find n propertiesByName)
        |> Seq.tryHead

    let private parseSchemaProperties (properties : JProperty list) : Result<JsonSchema, ParserError> =
        match tryFindPropertyWithInvalidName properties with
        | Some property -> ParserError.InvalidPropertyName property |> Error 
        | None ->
            let propertiesByName = keyPropertiesByName properties
            let jsonType = propertiesByName |> Map.tryFind "type" |> JsonTypeParser.parse
            jsonType
            |> Result.map (fun jsonType ->
                match jsonType with
                | JsonTypeParser.Object -> JsonSchema.Object { Properties = []; RequiredProperies = []; AdditionalProperties = true }
                | JsonTypeParser.Unset -> JsonSchema.Unvalidated)

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