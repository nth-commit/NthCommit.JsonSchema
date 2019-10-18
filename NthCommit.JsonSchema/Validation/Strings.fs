namespace NthCommit.JsonSchema.Validation

open NthCommit.JsonSchema.Domain

module Strings =

    let private validateInstanceIsValue (value : 'a) (instance : 'a) =
        JsonContextReader <| fun ctx -> seq {
            if value <> instance
            then yield SchemaError.Value {
                Path = ctx.CurrentPath.Render()
                Value = instance.ToString() } }

    let private validateInstanceInValues (values : Set<'a>) (instance : 'a) =
        JsonContextReader <| fun ctx -> seq {
            if values |> Set.contains instance |> not
            then yield SchemaError.Value {
                Path = ctx.CurrentPath.Render()
                Value = instance.ToString() } }

    let validate (schema : JsonStringSchema) (instance : string) =
        match schema with
        | JsonStringSchema.Const value -> validateInstanceIsValue value instance
        | JsonStringSchema.Enum values -> validateInstanceInValues values instance
        | JsonStringSchema.Unvalidated -> JsonContextReader.retn Seq.empty