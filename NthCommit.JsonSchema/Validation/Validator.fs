namespace NthCommit.JsonSchema.Validation

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper

[<AutoOpen>]
module Validator =

    let private toPrimitive = function
        | JTokenType.Null       -> JsonPrimitive.Null
        | JTokenType.Boolean    -> JsonPrimitive.Boolean
        | JTokenType.Integer    -> JsonPrimitive.Number
        | JTokenType.String     -> JsonPrimitive.String
        | JTokenType.Array      -> JsonPrimitive.Array
        | JTokenType.Object     -> JsonPrimitive.Object
        | x                     -> raise (Exception("Unhandled JTokenType: " + x.ToString()))

    let private reportTypeMismatch
        (schema : JsonElementSchema)
        (instance : JToken) =
            JsonContextReader <| fun ctx -> seq {
                SchemaError.Type {
                    Path = ctx.CurrentPath.Render()
                    ExpectedTypes = Set([schema.Primitive])
                    ActualType = instance.Type |> toPrimitive } }

    let rec private validateElement (schema : JsonElementSchema) (instance : JToken) =
        match (schema, matchJToken instance) with
        | JsonElementSchema.Null, JsonElementInstance.Null -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.Boolean, JsonElementInstance.Boolean _ -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.Number, JsonElementInstance.Integer _ -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.String s, JsonElementInstance.String i -> Strings.validate s i
        | JsonElementSchema.Array s, JsonElementInstance.Array i -> Arrays.validate validateElement s i
        | JsonElementSchema.Object s, JsonElementInstance.Object i -> Objects.validate validateElement s i
        | JsonElementSchema.Unvalidated, _ -> JsonContextReader.retn Seq.empty
        | _, _ -> reportTypeMismatch schema instance

    let private deserialize instanceJson =
        tryDeserialize<JToken> instanceJson
        |> Result.mapError (fun _ -> SchemaError.Json "")

    let validate (schema : JsonElementSchema) (instanceJson : string) : Result<JToken, List<SchemaError>> =
        match deserialize instanceJson with
        | Ok instance ->
            let ctx = {
                SchemaRoot = schema
                InstanceRoot = instance
                CurrentPath = JsonPath [] }
            match validateElement schema instance |> JsonContextReader.run ctx |> Seq.toList with
            | [] -> Ok instance
            | list -> Error list
        | Error e -> Error [e]