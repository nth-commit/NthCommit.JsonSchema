namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.JsonHelper

module JTokenType =

    let toPrimitive = function
        | JTokenType.String     -> JsonPrimitive.String
        | JTokenType.Boolean    -> JsonPrimitive.Boolean
        | JTokenType.Object     -> JsonPrimitive.Object
        | x                     -> raise (Exception("Unhandled JTokenType: " + x.ToString()))

module Validator =

    module private List =

        let toMap keyProjection list =
            list
            |> List.map (fun x -> (keyProjection x, x))
            |> Map

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
            | []    -> ""
            | _     ->
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

    let private validateInstanceInValues (instance : 'a) (values : 'a list) (ctx : JsonContext) = seq {
        if values |> List.contains instance |> not
        then yield SchemaError.Value {
            Path = ctx.CurrentPath.Render()
            Value = instance.ToString() } }

    let private validateString (schema : JsonStringSchema) (instance : string) (ctx : JsonContext) : seq<SchemaError> =
        match schema with
        | JsonStringSchema.Unvalidated  -> Seq.empty
        | JsonStringSchema.Enum values  -> validateInstanceInValues instance values ctx
        | JsonStringSchema.Const _      -> raise (Exception ("Unhandled: JsonStringSchema.Const"))

    let private evaluateReference (JsonReference reference) (ctx : JsonContext) : JsonSchema =
        match reference with
        | "#"   -> ctx.SchemaRoot
        | x     -> raise (NotImplementedException ("Reference is not supported: " + x))

    let private getEffectiveSchema (propertySchema : JsonPropertySchema) (ctx : JsonContext) : JsonSchema =
        match propertySchema with
        | Standard (_, schema)  -> schema
        | Reference reference   -> evaluateReference reference ctx

    let rec private validatePropertyAgainstSchema (schema : JsonSchema) (instance : JProperty) (ctx : JsonContext) = seq {
        let schemaPrimitive = schema.Primitive
        let instancePrimitive = instance.Value.Type |> JTokenType.toPrimitive
        if schemaPrimitive = instancePrimitive
        then yield! tokenMatchesSchema schema instance.Value ctx
        else yield SchemaError.Type {
            Path = instance.Path
            ExpectedTypes = Set([schemaPrimitive])
            ActualType = instancePrimitive } }

    and private validateProperty (schemas : JsonSchema list) (instance : JProperty) (ctx : JsonContext) : seq<SchemaError> =
        schemas
        |> List.map (fun s -> validatePropertyAgainstSchema s instance ctx)
        |> Seq.concat

    and private objectMatchesSchema (schema : JsonObjectSchema) (instance : JProperty list) (ctx : JsonContext) : seq<SchemaError> =
        let schemaPropertiesByName =
            schema.Properties
            |> List.toMap (fun p -> p.Name)

        let getSpecificPropertySchema (instanceProperty : JProperty) : JsonSchema option =
            schemaPropertiesByName
            |> Map.tryFind (instanceProperty.Name)
            |> Option.map (fun propertySchema -> getEffectiveSchema propertySchema ctx)

        let getPatternPropertySchemas (instanceProperty : JProperty) : JsonSchema list =
            schema.PatternProperties
            |> List.filter (fun (regex, _) -> regex.IsMatch(instanceProperty.Name))
            |> List.map (fun (_, s) -> getEffectiveSchema s ctx)

        let getPropertySchemas (instanceProperty : JProperty) =
            getSpecificPropertySchema instanceProperty
            |> Option.toList
            |> List.append (getPatternPropertySchemas instanceProperty)

        instance
        |> List.map (fun p -> (getPropertySchemas p, p))
        |> List.map (fun (propertySchemas, instanceProperty) ->
            validateProperty propertySchemas instanceProperty (ctx.PushProperty instanceProperty.Name))
        |> Seq.concat

    and private tokenMatchesSchema (schema : JsonSchema) (instance : JToken) (ctx : JsonContext) : seq<SchemaError> = seq {
        match (schema, matchJToken instance) with
        | JsonSchema.Object objectSchema, MatchedJObject objectInstance ->
            yield! objectMatchesSchema objectSchema objectInstance ctx
        | JsonSchema.String stringSchema, MatchedJValueAsString stringInstance ->
            yield! validateString stringSchema stringInstance ctx
        | _, _ ->
            let expectedTypes = Set([JsonPrimitive.Object])
            let actualType = instance.Type |> JTokenType.toPrimitive
            yield SchemaError.Type {
                Path = ctx.CurrentPath.Render()
                ExpectedTypes = expectedTypes
                ActualType = actualType } }

    let validate (schema : JsonSchema) (instance : JToken) : List<SchemaError> =
        {   SchemaRoot = schema
            InstanceRoot = instance
            CurrentPath = JsonPath [] }
        |> tokenMatchesSchema schema instance
        |> Seq.toList