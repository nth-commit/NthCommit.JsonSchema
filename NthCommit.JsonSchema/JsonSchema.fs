namespace NthCommit.JsonSchema

open System
open NthCommit.JsonSchema.Validation
open NthCommit.JsonSchema.Parsing

type ValidationFailureTarget =
    | Schema    = 1
    | Instance  = 2

type ValidationFailureType =
    | InvalidJson       = 1
    | SchemaViolation   = 2

type ValidationFailure = {
    Target   : ValidationFailureTarget
    Type     : ValidationFailureType
    Message  : string
    Path     : string }

module JsonSchema =

    module private ValidationFailure =

        let private targetString = function
            | ValidationFailureTarget.Schema    -> "schema"
            | ValidationFailureTarget.Instance  -> "instance"
            | _                                 -> raise (Exception ("Unhandled"))

        let buildInvalidJsonMessage target =
            sprintf "Error deserializing %s: Invalid JSON" (targetString target)

        let fromInvalidJson target = {
            Target  = target
            Type    = ValidationFailureType.InvalidJson
            Message = buildInvalidJsonMessage target
            Path    = "" }

        let private buildSchemaTypeErrorMessage (error : SchemaTypeError) =
            sprintf
                @"expected type of %s but received %s at path ""%s"""
                ((error.ExpectedTypes |> Seq.exactlyOne).ToString())
                (error.ActualType.ToString())
                error.Path

        let private buildSchemaErrorMessage prefix error =
            match error with
            | SchemaError.Json _ -> buildInvalidJsonMessage ValidationFailureTarget.Instance
            | SchemaError.Type e    -> buildSchemaTypeErrorMessage e
            | SchemaError.Value _   -> "value error"
            | _ -> "unknown error"
            |> sprintf "%s: %s" prefix

        let private resolveSchemaErrorPath = function
            | SchemaError.Json p -> p
            | SchemaError.Type e -> e.Path
            | SchemaError.Value e -> e.Path
            | SchemaError.Required e -> e.Path

        let private resolveSchemaErrorType = function
            | SchemaError.Json _ -> ValidationFailureType.InvalidJson
            | SchemaError.Type _
            | SchemaError.Value _
            | SchemaError.Required _ -> ValidationFailureType.SchemaViolation

        let fromSchemaError target prefix error = {
            Target  = target
            Type    = resolveSchemaErrorType error
            Message = buildSchemaErrorMessage prefix error
            Path    = resolveSchemaErrorPath error }

        let private SCHEMA_ERROR_IN_PARSING_SCHEMA =
            "Error validating schema: Schema JSON does not conform to JSON Schema Standard"

        let schemaNotConformingToStandard =  fromSchemaError ValidationFailureTarget.Schema SCHEMA_ERROR_IN_PARSING_SCHEMA

        let fromParserError = function
            | ParserError.Schema schemaError    -> schemaNotConformingToStandard schemaError

        let private SCHEMA_ERROR_IN_INSTANCE = "Error validating instance"

        let fromInstanceSchemaError = fromSchemaError ValidationFailureTarget.Instance SCHEMA_ERROR_IN_INSTANCE

    let private parseSchema schemaJson =
        match Parser.parse schemaJson with
        | Ok dom    -> Ok dom
        | Error e   -> Error [ValidationFailure.fromParserError e]

    let private validateInstance schema instanceJson =
        match Validator.validate schema instanceJson with
        | Ok _ -> []
        | Error errors -> errors |> List.map ValidationFailure.fromInstanceSchemaError

    [<CompiledName("Validate")>]
    let validate schemaJson instanceJson =
        match parseSchema schemaJson with
        | Ok schema -> validateInstance schema instanceJson
        | Error errors  -> errors
        |> System.Collections.Generic.List