namespace NthCommit.JsonSchema.Validation

open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver

type SchemaTypeError = {
    Path            : string
    ExpectedTypes   : Set<JsonPrimitive>
    ActualType      : JsonPrimitive }

type SchemaValueError = {
    Path    : string
    Value   : string }

type SchemaRequiredError = {
    Path : string
    RequiredPropertyName : string }

type SchemaAdditionError = {
    Path : string
    AdditionalPropertyName : string }

[<RequireQualifiedAccess>]
type SchemaError =
    | Json of JsonDeserializationError
    | Type of SchemaTypeError
    | Value of SchemaValueError
    | Required of SchemaRequiredError
    | Additional of SchemaAdditionError
