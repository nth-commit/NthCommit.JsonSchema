namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.JsonHelper
open NthCommit.JsonSchema.Parsing

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

    let rec concat (list : List<Result<'T, 'TError>>) : Result<List<'T>, 'TError> =
        match list with
        | [] -> Ok []
        | x :: xs ->
            match x with
            | Ok x' -> concat xs |> Result.map (fun xs' -> x' :: xs')
            | Error e -> e |> Error

[<AutoOpen>]
module Parser =

    let private keyPropertiesByName (properties : JProperty list) : Map<string, JProperty> =
        properties
        |> List.toMap (fun p -> p.Name)

    let private tryFindPropertyWithInvalidName (properties : JProperty list) : JProperty option =
        let propertiesByName = keyPropertiesByName properties
        Set.difference (propertiesByName |> Map.keys) (Set(["type"; "properties"]))
        |> Seq.map (fun n -> Map.find n propertiesByName)
        |> Seq.tryHead

    let rec private parseSchema (properties : JProperty list) : Result<JsonSchema, ParserError> =
        match tryFindPropertyWithInvalidName properties with
        | Some property -> Errors.signalPropertyName property.Path ""
        | None ->
            let propertiesByName = keyPropertiesByName properties
            propertiesByName |> Map.tryFind "type" |> SchemaTypeParser.parse
            |> Result.append (propertiesByName |> Map.tryFind "properties" |> SchemaPropertiesParser.parse)
            |> Result.bind (fun (jsonType, schemaProperties) ->
                match jsonType with
                | SchemaType.Null -> JsonSchema.Null |> Ok
                | SchemaType.String -> JsonSchema.String |> Ok
                | SchemaType.Number -> JsonSchema.Number |> Ok
                | SchemaType.Boolean -> JsonSchema.Boolean |> Ok
                | SchemaType.Array -> JsonSchema.Array (JsonSchema.Unvalidated) |> Ok
                | SchemaType.Object ->
                    match schemaProperties with
                    | SchemaProperties.Unset ->
                        JsonSchema.Object { Properties = []; Required = []; AdditionalProperties = true } |> Ok
                    | SchemaProperties.UnevaluatedSubSchemas unevaluatedSubSchemas ->
                        unevaluatedSubSchemas
                        |> List.map (snd >> parseSchema)
                        |> Result.concat
                        |> Result.map (fun subSchemas ->
                            let properties = subSchemas |> List.zip (unevaluatedSubSchemas |> List.map fst)
                            JsonSchema.Object { Properties = properties; Required = []; AdditionalProperties = true })
                | SchemaType.Unset -> JsonSchema.Unvalidated |> Ok)

    let private parseSchemaToken (token : JsonToken) : Result<JsonSchema, ParserError> =
        match token with
        | JsonObject properties -> parseSchema properties
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
