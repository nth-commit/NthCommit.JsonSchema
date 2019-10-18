namespace NthCommit.JsonSchema

open System
open NthCommit.JsonSchema.Validation
open NthCommit.JsonSchema.Parsing
open JsonHelper

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

    module private Result =

        let combine (result : Result<'a,'a>) : 'a =
            match result with
            | Ok ok -> ok
            | Error error -> error

    module private ValidationFailure =


        let private resolveSchemaErrorPath = function
            | SchemaError.Json e -> e.Path
            | SchemaError.Type e -> e.Path
            | SchemaError.Value e -> e.Path
            | SchemaError.Required e -> e.Path
            | SchemaError.Additional e -> e.Path

        let private renderInvalidJson (e : JsonDeserializationError) =
            sprintf "JSON was invalid. Path '%s', line %i, position %i" e.Path e.LineNumber e.LinePosition

        let private renderInvalidType (e : SchemaTypeError) =
            let literalizeType (p : JsonPrimitive) = sprintf "'%s'" (p.ToSchemaTypeValue())
            let literalizeTypes (ps : JsonPrimitive seq) =
                ps
                |> Seq.map literalizeType
                |> Seq.reduce (fun acc curr -> sprintf "%s, %s" acc curr)
            sprintf
                "Expected type of [%s], but found %s"
                (literalizeTypes e.ExpectedTypes)
                (literalizeType e.ActualType)

        let private renderInvalidValue (e : SchemaValueError) =
            sprintf
                "Expected value to be in ['bar'], but found '%s'"
                e.Value

        let private renderRequiredProperty (e : SchemaRequiredError) =
            sprintf
                "Property '%s' was required"
                e.RequiredPropertyName

        let private renderAdditionalProperty (e : SchemaAdditionError) =
            sprintf
                "Additional property '%s' was detected"
                e.AdditionalPropertyName

        let private renderErrorBody = function
            | SchemaError.Json e -> renderInvalidJson e
            | SchemaError.Type e -> renderInvalidType e
            | SchemaError.Value e -> renderInvalidValue e
            | SchemaError.Required e -> renderRequiredProperty e
            | SchemaError.Additional e -> renderAdditionalProperty e

        let private renderErrorPrefix = function
            | ValidationFailureTarget.Schema -> "Invalid schema"
            | ValidationFailureTarget.Instance -> "Invalid instance"
            | _ -> raise (Exception ("Unhandled value"))

        let private renderErrorMessage (target : ValidationFailureTarget) (error : SchemaError) =
            sprintf
                "%s: %s"
                (renderErrorPrefix target)
                (renderErrorBody error)

        let private getErrorType = function
            | SchemaError.Json _ -> ValidationFailureType.InvalidJson
            | SchemaError.Type _
            | SchemaError.Value _
            | SchemaError.Required _
            | SchemaError.Additional _
                -> ValidationFailureType.SchemaViolation

        let make (target : ValidationFailureTarget) (error : SchemaError) : ValidationFailure =
            {   Target = target
                Type = getErrorType error
                Message = renderErrorMessage target error
                Path = resolveSchemaErrorPath error }

    let private parseSchema schemaJson =
        Parser.parse schemaJson
        |> Result.mapError (List.map (ValidationFailure.make ValidationFailureTarget.Schema))

    let private validateInstance schema instanceJson =
        Validator.validate schema instanceJson
        |> Result.map (fun _ -> [])
        |> Result.mapError (List.map (ValidationFailure.make ValidationFailureTarget.Instance))

    [<CompiledName("Validate")>]
    let validate schemaJson instanceJson =
        parseSchema schemaJson
        |> Result.bind (fun schema -> validateInstance schema instanceJson)
        |> Result.combine
        |> System.Collections.Generic.List
