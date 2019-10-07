namespace NthCommit.JsonSchema.Parsing

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.JsonHelper

[<RequireQualifiedAccess>]
type SchemaType =
    | Null
    | String
    | Number
    | Boolean
    | Object
    | Array
    | Unset

module SchemaTypeParser =

    let private matchSchemaType = function
        | "null" -> SchemaType.Null |> Some
        | "string" -> SchemaType.String |> Some
        | "number" -> SchemaType.Number |> Some
        | "boolean" -> SchemaType.Boolean |> Some
        | "array" -> SchemaType.Array |> Some
        | "object" -> SchemaType.Object |> Some
        | _ -> None

    let parse (propertyOpt : JProperty option) : Result<SchemaType, ParserError> =
        match propertyOpt with
        | Some property ->
            match matchJToken property.Value with
            | MatchedJValueAsString s ->
                match matchSchemaType s with
                | Some schemaType -> Ok schemaType
                | None -> Errors.signalPropertyValue property.Path
            | _ -> Errors.signalOnePropertyType property.Path "string"
        | None _ -> SchemaType.Unset |> Ok
