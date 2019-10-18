namespace NthCommit.JsonSchema.Validation

open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver

[<AutoOpen>]
module Validator =

    let private reportTypeMismatch
        (schema : JsonElementSchema)
        (instance : JsonDriverElement) =
            JsonContextReader <| fun ctx -> seq {
                SchemaError.Type {
                    Path = ctx.CurrentPath.Render()
                    ExpectedTypes = Set([schema.Primitive])
                    ActualType = instance.Primitive } }

    let rec private validateElement (schema : JsonElementSchema) (instance : JsonDriverElement) =
        match (schema, instance.Match()) with
        | JsonElementSchema.Null, JsonElementInstance.Null -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.Boolean, JsonElementInstance.Boolean _ -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.Number, JsonElementInstance.Integer _ -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.String s, JsonElementInstance.String i -> Strings.validate s i
        | JsonElementSchema.Array s, JsonElementInstance.Array i -> Arrays.validate validateElement s i
        | JsonElementSchema.Object s, JsonElementInstance.Object i -> Objects.validate validateElement s i
        | JsonElementSchema.Unvalidated, _ -> JsonContextReader.retn Seq.empty
        | _, _ -> reportTypeMismatch schema instance

    let validate (schema : JsonElementSchema) (instanceJson : string) : Result<JsonDriverElement, List<SchemaError>> =
        let driver : JsonDriver = new NewtonsoftJsonDriver() :> JsonDriver
        match driver.GetElement(instanceJson) with
        | Ok instance ->
            let ctx = {
                SchemaRoot = schema
                InstanceRoot = instance.Match()
                CurrentPath = JsonPath [] }
            match validateElement schema instance |> JsonContextReader.run ctx |> Seq.toList with
            | [] -> Ok instance
            | list -> Error list
        | Error e -> Error [SchemaError.Json e]