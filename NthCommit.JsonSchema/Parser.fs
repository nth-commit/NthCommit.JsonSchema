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

    let rec fold
        (folder : 'State -> 'T -> Result<'State, 'TError>)
        (state : 'State)
        (list : List<'T>) =
            match list with
            | [] -> Ok state
            | x :: xs ->
                match folder state x with
                | Ok nextState -> fold folder nextState xs
                | Error e -> Error e

    let rec concat (list : List<Result<'T, 'TError>>) : Result<List<'T>, 'TError> =
        match list with
        | [] -> Ok []
        | x :: xs ->
            match x with
            | Ok x' -> concat xs |> Result.map (fun xs' -> x' :: xs')
            | Error e -> e |> Error

[<AutoOpen>]
module Parser =

    type InvalidPropertyNameData = {
        Path : string
        Trivia : string }

    type InvalidPropertyTypeData = {
        Path : string
        AcceptedTypes : string list }

    [<RequireQualifiedAccess>]
    type ParserError =
        | Json
        | PropertyName of InvalidPropertyNameData
        | PropertyType of InvalidPropertyTypeData
        | PropertyValue of string

    module Errors =

        let signalPropertyName path trivia =
            { Path = path; Trivia = trivia }
            |> ParserError.PropertyName
            |> Error

        let signalPropertyType path acceptedTypes =
            { Path = path; AcceptedTypes = acceptedTypes }
            |> ParserError.PropertyType
            |> Error

        let signalOnePropertyType path acceptedType =
            signalPropertyType path [acceptedType]

        let signalPropertyValue path =
            path
            |> ParserError.PropertyValue
            |> Error

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

        let private matchJsonType = function
            | "null" -> JsonType.Null |> Some
            | "string" -> JsonType.String |> Some
            | "number" -> JsonType.Number |> Some
            | "boolean" -> JsonType.Boolean |> Some
            | "array" -> JsonType.Array |> Some
            | "object" -> JsonType.Object |> Some
            | _ -> None

        let parse (propertyOpt : JProperty option) : Result<JsonType, ParserError> =
            match propertyOpt with
            | Some property ->
                match matchJToken property.Value with
                | JsonString s ->
                    match matchJsonType s with
                    | Some jsonType -> Ok jsonType
                    | None -> Errors.signalPropertyValue property.Path
                | _ -> Errors.signalOnePropertyType property.Path "string"
            | None _ -> JsonType.Unset |> Ok

    module PropertiesParser =

        module private Errors =
            let signalPropertiesMisplaced path =
                Errors.signalPropertyName path "Property 'properties' is only valid when 'type' is 'object'"

        [<RequireQualifiedAccess>]
        type Properties =
            | SubSchemas of (string * JProperty list) list
            | Unset

        let private parsePropertiesItem (property : JProperty) : Result<string * JProperty list, ParserError> =
            match matchJToken property.Value with
            | JsonToken.JsonObject properties -> Ok (property.Name, properties)
            | _ -> Errors.signalOnePropertyType property.Path "object"

        let private parsePropertiesList (properties : JProperty list) : Result<Properties, ParserError> =
            let folder acc curr =
                parsePropertiesItem curr
                |> Result.map (fun property -> property :: acc) 
            Result.fold folder [] properties
            |> Result.map Properties.SubSchemas

        let private parsePropertiesValue path value =
            match matchJToken value with
            | JsonToken.JsonObject properties -> parsePropertiesList properties
            | _ -> Errors.signalOnePropertyType path "object"

        let parse (propertyOpt : JProperty option) (currentJsonType : JsonTypeParser.JsonType) : Result<Properties, ParserError> =
            match propertyOpt with
            | Some property ->
                match currentJsonType with
                | JsonTypeParser.JsonType.Object -> parsePropertiesValue property.Path property.Value
                | _ -> Errors.signalPropertiesMisplaced property.Path
            | None -> Properties.Unset |> Ok

    let private keyPropertiesByName (properties : JProperty list) : Map<string, JProperty> =
        properties
        |> List.toMap (fun p -> p.Name)

    let private tryFindPropertyWithInvalidName (properties : JProperty list) : JProperty option =
        let propertiesByName = keyPropertiesByName properties
        Set.difference (propertiesByName |> Map.keys) (Set(["type"; "properties"]))
        |> Seq.map (fun n -> Map.find n propertiesByName)
        |> Seq.tryHead

    let rec private parseSchemaProperties (properties : JProperty list) : Result<JsonSchema, ParserError> =
        match tryFindPropertyWithInvalidName properties with
        | Some property -> Errors.signalPropertyName property.Path ""
        | None ->
            let propertiesByName = keyPropertiesByName properties
            propertiesByName |> Map.tryFind "type" |> JsonTypeParser.parse
            |> Result.append (propertiesByName |> Map.tryFind "properties" |> PropertiesParser.parse)
            |> Result.bind (fun (jsonType, schemaProperties) ->
                match jsonType with
                | JsonTypeParser.JsonType.Null -> JsonSchema.Null |> Ok
                | JsonTypeParser.JsonType.String -> JsonSchema.String |> Ok
                | JsonTypeParser.JsonType.Number -> JsonSchema.Number |> Ok
                | JsonTypeParser.JsonType.Boolean -> JsonSchema.Boolean |> Ok
                | JsonTypeParser.JsonType.Array -> JsonSchema.Array (JsonSchema.Unvalidated) |> Ok
                | JsonTypeParser.JsonType.Object ->
                    match schemaProperties with
                    | PropertiesParser.Properties.Unset ->
                        JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true } |> Ok
                    | PropertiesParser.Properties.SubSchemas untraversedSubSchemas ->
                        untraversedSubSchemas
                        |> List.map (snd >> parseSchemaProperties)
                        |> Result.concat
                        |> Result.map (fun subSchemas ->
                            let properties = subSchemas |> List.zip (untraversedSubSchemas |> List.map fst)
                            JsonSchema.Object { Properties = properties; Required = []; AdditionalProperties = true })
                | JsonTypeParser.JsonType.Unset -> JsonSchema.Unvalidated |> Ok)

    let private parseSchemaToken (token : JsonToken) : Result<JsonSchema, ParserError> =
        match token with
        | JsonObject properties -> parseSchemaProperties properties
        | _ -> raise (Exception "")

    let private parseSchemaElement schemaToken : Result<JsonSchema, ParserError> =
        matchJToken schemaToken
        |> parseSchemaToken

    let private deserialize schema =
        tryDeserialize schema
        |> Result.mapError (fun _ -> ParserError.Json)

    let parse (schema : string) : Result<JsonSchema, ParserError> =
        deserialize schema
        |> Result.bind parseSchemaElement