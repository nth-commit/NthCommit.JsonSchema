namespace NthCommit.JsonSchema.Parsing

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.JsonHelper

[<RequireQualifiedAccess>]
type SchemaProperties =
    | UnevaluatedSubSchemas of (string * JProperty list) list
    | Unset

module private Result =
    let rec fold
        (folder : 'State -> 'T -> Result<'State, 'TError>)
        (state : 'State)
        (list : List<'T>) =
            match list with
            | [] -> Ok state
            | x :: xs ->
                folder state x
                |> Result.bind (fun nextState -> fold folder nextState xs)

module SchemaPropertiesParser =

    module private Errors =
        let signalPropertiesMisplaced path =
            Errors.signalPropertyName path "Property 'properties' is only valid when 'type' is 'object'"

    let private parseSubSchema (property : JProperty) : Result<string * JProperty list, ParserError> =
        match matchJToken property.Value with
        | JsonToken.JsonObject properties -> Ok (property.Name, properties)
        | _ -> Errors.signalOnePropertyType property.Path "object"

    let private parseSubSchemas (properties : JProperty list) : Result<SchemaProperties, ParserError> =
        let folder acc curr =
            parseSubSchema curr
            |> Result.map (fun property -> property :: acc) 
        Result.fold folder [] properties
        |> Result.map SchemaProperties.UnevaluatedSubSchemas

    let private parseSchemaProperties path value =
        match matchJToken value with
        | JsonToken.JsonObject properties -> parseSubSchemas properties
        | _ -> Errors.signalOnePropertyType path "object"

    let parse (propertyOpt : JProperty option) (currentSchemaType : SchemaType) : Result<SchemaProperties, ParserError> =
        match propertyOpt with
        | Some property ->
            match currentSchemaType with
            | SchemaType.Object -> parseSchemaProperties property.Path property.Value
            | _ -> Errors.signalPropertiesMisplaced property.Path
        | None -> SchemaProperties.Unset |> Ok
