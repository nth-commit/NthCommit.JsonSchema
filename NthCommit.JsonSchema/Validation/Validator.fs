namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.JsonHelper

module JTokenType =

    let toPrimitive = function
        | JTokenType.String -> JsonPrimitive.String
        | JTokenType.Boolean -> JsonPrimitive.Boolean
        | JTokenType.Object -> JsonPrimitive.Object
        | x -> raise (Exception("Unhandled JTokenType: " + x.ToString()))

module private Result =

    let rec allOk (list : List<Result<_, 'TError>>) : Result<unit, 'TError> =
        match list with
        | []        -> Ok ()
        | x :: xs   -> x |> Result.bind (fun _ -> allOk xs)

module Validator =

    module private List =

        let toMap keyProjection list =
            list
            |> List.map (fun x -> (keyProjection x, x))
            |> Map

        let mapFst f list =
            list
            |> List.map (fun (x, y) -> (f x, y))

    type JsonPathComponent =
        | PropertyAccess of string
        | ArrayAccess of int

    type JsonPath =
        | JsonPath of JsonPathComponent list

        member this.Push (newComponent : JsonPathComponent) =
            let (JsonPath components) = this
            components
            |> List.append [newComponent]
            |> JsonPath

        member this.Render () =
            let (JsonPath components) = this
            match components with
            | [] -> ""
            | _ ->
                components
                |> List.map (fun c ->
                    match c with
                    | PropertyAccess p -> p
                    | ArrayAccess _ -> raise (Exception ("Unhandled: ArrayAccess when rendering component")))
                |> List.reduce (sprintf "%s.%s")

    type JsonContext =
        {   SchemaRoot : JsonSchema
            InstanceRoot : JToken
            CurrentPath : JsonPath }
            member this.PushProperty property =
                { this with CurrentPath = this.CurrentPath.Push (PropertyAccess property) }

    type SchemaTypeError = {
        Path : string
        ExpectedTypes : Set<JsonPrimitive>
        ActualType : JsonPrimitive }
    
    type SchemaValueError = {
        Path : string
        Value : string }
    
    [<RequireQualifiedAccess>]
    type SchemaError =
        | Type of SchemaTypeError
        | Value of SchemaValueError

    type JsonValidationEvent =
        | BeginValidatingObject
        | EndValidatingObject
        | BeginValidatingProperty of string
        | EndValidatingProperty
        | ValidatedString of string

    let private validateInstanceInValues (instance : 'a) (values : 'a list) (ctx : JsonContext) =
        if values |> List.contains instance
        then Ok ()
        else Error <| SchemaError.Value {
            Path = ctx.CurrentPath.Render()
            Value = instance.ToString() }

    let private validateString (schema : JsonStringSchema) (instance : string) (ctx : JsonContext) : Result<unit, SchemaError> =
        match schema with
        | JsonStringSchema.Unvalidated  -> Ok ()
        | JsonStringSchema.Enum values  -> validateInstanceInValues instance values ctx
        | JsonStringSchema.Const _      -> raise (Exception ("Unhandled: JsonStringSchema.Const"))

    let rec private validateProperty (schema : JsonPropertySchema) (instance : JProperty) (ctx : JsonContext) : Result<unit, SchemaError> =
        let schemaPrimitive = schema.Value.Primitive
        let instancePrimitive = instance.Value.Type |> JTokenType.toPrimitive
        if schemaPrimitive = instancePrimitive
        then
            match schema with
            | Standard (_, valueSchema) -> tokenMatchesSchema valueSchema instance.Value ctx
            | _                         -> raise (Exception ())
        else Error <| SchemaError.Type {
            Path = instance.Path
            ExpectedTypes = Set([schemaPrimitive])
            ActualType = instancePrimitive }

    and private objectMatchesSchema (schema : JsonObjectSchema) (instance : JProperty list) (ctx : JsonContext) : Result<unit, SchemaError> =
        let schemaPropertiesByName =
            schema.Properties
            |> List.toMap (fun p -> p.Name)
        instance
        |> List.map (fun p -> (schemaPropertiesByName |> Map.tryFind (p.Name), p))
        |> List.filter (fst >> Option.isSome)
        |> List.mapFst Option.get
        |> List.map (fun (schemaProperty, instanceProperty) ->
            validateProperty schemaProperty instanceProperty (ctx.PushProperty schemaProperty.Name))
        |> Result.allOk

    and private tokenMatchesSchema (schema : JsonSchema) (instance : JToken) (ctx : JsonContext) : Result<unit, SchemaError> =
        match (schema, matchJToken instance) with
        | JsonSchema.Object objectSchema, MatchedJObject objectInstance ->
            objectMatchesSchema objectSchema objectInstance ctx
        | JsonSchema.String stringSchema, MatchedJValueAsString stringInstance ->
            validateString stringSchema stringInstance ctx
        | _, _ ->
            let expectedTypes = Set([JsonPrimitive.Object])
            let actualType = instance.Type |> JTokenType.toPrimitive
            SchemaError.Type {
                Path = ctx.CurrentPath.Render()
                ExpectedTypes = expectedTypes
                ActualType = actualType } |> Error

    let validate (schema : JsonSchema) (instance : JToken) : Result<unit, SchemaError> =
        {   SchemaRoot = schema
            InstanceRoot = instance
            CurrentPath = JsonPath [] }
        |> tokenMatchesSchema schema instance