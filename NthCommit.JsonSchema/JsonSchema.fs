namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq

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

        let fromInvalidJson target = {
            Target  = target
            Type    = ValidationFailureType.InvalidJson
            Message = sprintf "Error deserializing %s: Invalid JSON" (targetString target)
            Path    = "" }

        let private buildSchemaTypeErrorMessage (error : SchemaTypeError) =
            sprintf
                @"expected type of %s but recieved %s at path ""%s"""
                ((error.ExpectedTypes |> Seq.exactlyOne).ToString())
                (error.ActualType.ToString())
                error.Path

        let private buildSchemaErrorMessage prefix error =
            match error with
            | SchemaError.Type e    -> buildSchemaTypeErrorMessage e
            | SchemaError.Value _   -> "value error"
            |> sprintf "%s: %s" prefix

        let private resolveSchemeErrorPath = function
            | SchemaError.Type e    -> e.Path
            | SchemaError.Value e   -> e.Path

        let fromSchemaError target prefix error = {
            Target  = target
            Type    = ValidationFailureType.SchemaViolation
            Message = buildSchemaErrorMessage prefix error
            Path    = resolveSchemeErrorPath error }

        let private SCHEMA_ERROR_IN_PARSING_SCHEMA =
            "Error validating schema: Schema JSON does not conform to JSON Schema Standard"

        let schemaJsonInvalid = fromInvalidJson ValidationFailureTarget.Schema

        let schemaNotConformingToStandard =  fromSchemaError ValidationFailureTarget.Schema SCHEMA_ERROR_IN_PARSING_SCHEMA

        let fromParserError = function
            | ParserError.Json                  -> schemaJsonInvalid
            | ParserError.Schema schemaError    -> schemaNotConformingToStandard schemaError
            | _ -> raise (Exception ("Unexpected error"))

        let private SCHEMA_ERROR_IN_INSTANCE = "Error validating instance"

        let instanceJsonInvalid = fromInvalidJson ValidationFailureTarget.Instance

        let fromInstanceSchemaError = fromSchemaError ValidationFailureTarget.Instance SCHEMA_ERROR_IN_INSTANCE

    let private parseSchema schemaJson =
        match Parser.parse schemaJson with
        | Ok dom    -> Ok dom
        | Error e   -> Error [ValidationFailure.fromParserError e]

    let private deserializeInstance instanceJson =
        match JsonHelper.tryDeserialize<JToken> instanceJson with
        | Ok instanceToken  -> Ok instanceToken
        | Error _           -> Error [ValidationFailure.instanceJsonInvalid]

    [<CompiledName("Validate")>]
    let validate schemaJson instanceJson =
        match parseSchema schemaJson with
        | Ok schema ->
            match deserializeInstance instanceJson with
            | Ok instanceToken ->
                Validator.validate schema instanceToken
                |> List.map ValidationFailure.fromInstanceSchemaError
            | Error errors -> errors
        | Error errors  -> errors
        |> System.Collections.Generic.List