namespace NthCommit.JsonSchema.Parsing

open System
open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver
open NthCommit.JsonSchema.Validation

[<AutoOpen>]
module Parser =

    module private List =

        let toMap keyProjection list =
            list
            |> List.map (fun x -> (keyProjection x, x))
            |> Map

    let private META_SCHEMA = JsonElementSchema.Object {
        Properties = [
            JsonPropertySchema.Inline (
                "type",
                JsonElementSchema.String <|
                    JsonStringSchema.Enum (Set(["null"; "boolean"; "number"; "string"; "object"; "array"])))
            JsonPropertySchema.Inline (
                "properties",
                JsonElementSchema.Object {
                    Properties = []
                    PatternProperties = [
                        (RegularExpression ".*", [ JsonPropertySchema.Reference <| JsonReference "#" ])]
                    Required = Set []
                    AdditionalProperties = true })]
        PatternProperties = []
        Required = Set []
        AdditionalProperties = true }

    let private tryGetTypeProperty (objectInstance : JsonObjectInstance) =
        objectInstance.TryFindProperty "type"
        |> Option.map (fun p ->
            p.Value |> JsonDriverElement.getString)

    let private parseSchemaElementOfType
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (objectInstance : JsonObjectInstance) = function
            | "null" -> JsonElementSchema.Null
            | "boolean" -> JsonElementSchema.Boolean
            | "number" -> JsonElementSchema.Number
            | "string" -> Strings.parse objectInstance
            | "array" -> Arrays.parse parseSchemaToken objectInstance
            | "object" -> Objects.parse parseSchemaToken objectInstance
            | _ -> raise (Exception ("Unexpected token"))

    let rec private parseSchemaElement (element : JsonDriverElement) : JsonElementSchema =
        let objectInstance = JsonDriverElement.getObject element
        match tryGetTypeProperty objectInstance with
        | Some jtype -> parseSchemaElementOfType parseSchemaElement objectInstance jtype
        | None -> JsonElementSchema.Unvalidated

    let parse (schema : string) : Result<JsonElementSchema, List<SchemaError>> =
        match validate META_SCHEMA schema with
        | Ok schemaToken -> parseSchemaElement schemaToken |> Ok
        | Error errors -> errors |> Error
