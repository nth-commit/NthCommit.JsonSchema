namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper

type SchemaTypeError = {
    Path            : string
    ExpectedTypes   : Set<JsonPrimitive>
    ActualType      : JsonPrimitive }

type SchemaValueError = {
    Path    : string
    Value   : string }

[<RequireQualifiedAccess>]
type SchemaError =
    | Type  of SchemaTypeError
    | Value of SchemaValueError

module JTokenType =

    let toPrimitive = function
        | JTokenType.Null       -> JsonPrimitive.Null
        | JTokenType.Boolean    -> JsonPrimitive.Boolean
        | JTokenType.Integer    -> JsonPrimitive.Number
        | JTokenType.String     -> JsonPrimitive.String
        | JTokenType.Array      -> JsonPrimitive.Array
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
            | []    -> "#"
            | _     ->
                components
                |> List.rev
                |> List.map (fun c ->
                    match c with
                    | PropertyAccess p  -> p
                    | ArrayAccess _     -> raise (Exception ("Unhandled: ArrayAccess when rendering component")))
                |> List.reduce (sprintf "%s.%s")

    type JsonContext =
        {   SchemaRoot : JsonSchemaElement
            InstanceRoot : JToken
            CurrentPath : JsonPath }
            member this.PushProperty property =
                { this with CurrentPath = this.CurrentPath.Push (PropertyAccess property) }

    let private reportTypeMismatch (schema : JsonSchemaElement) (instance : JToken) (ctx : JsonContext) =
        SchemaError.Type {
            Path            = ctx.CurrentPath.Render()
            ExpectedTypes   = Set([schema.Primitive])
            ActualType      = instance.Type |> JTokenType.toPrimitive }

    let private validateInstanceIsValue (instance : 'a) (value : 'a) (ctx : JsonContext) = seq {
        if value <> instance
        then yield SchemaError.Value {
            Path = ctx.CurrentPath.Render()
            Value = instance.ToString() } }

    let private validateInstanceInValues (instance : 'a) (values : 'a list) (ctx : JsonContext) = seq {
        if values |> List.contains instance |> not
        then yield SchemaError.Value {
            Path = ctx.CurrentPath.Render()
            Value = instance.ToString() } }

    let private stringMatchesSchema (schema : JsonSchemaString) (instance : string) (ctx : JsonContext) : seq<SchemaError> =
        match schema with
        | JsonSchemaString.Unvalidated  -> Seq.empty
        | JsonSchemaString.Const value  -> validateInstanceIsValue instance value ctx
        | JsonSchemaString.Enum values  -> validateInstanceInValues instance values ctx

    let private evaluateReference (JsonReference reference) (ctx : JsonContext) : JsonSchemaElement =
        match reference with
        | "#"   -> ctx.SchemaRoot
        | x     -> raise (NotImplementedException ("Reference is not supported: " + x))

    let private getEffectiveSchema (propertySchema : JsonSchemaObjectProperty) (ctx : JsonContext) : JsonSchemaElement =
        match propertySchema with
        | Inline (_, schema)  -> schema
        | Reference reference   -> evaluateReference reference ctx

    let rec private validateProperty (schemas : JsonSchemaElement list) (instance : JProperty) (ctx : JsonContext) : seq<SchemaError> =
        schemas
        |> List.map (fun s -> tokenMatchesSchema s instance.Value ctx)
        |> Seq.concat

    and private objectMatchesSchema (schema : JsonSchemaObject) (instance : JProperty list) (ctx : JsonContext) : seq<SchemaError> =
        let schemaPropertiesByName =
            schema.Properties
            |> List.toMap (fun p -> p.Name)

        let getSpecificPropertySchema (instanceProperty : JProperty) : JsonSchemaElement option =
            schemaPropertiesByName
            |> Map.tryFind (instanceProperty.Name)
            |> Option.map (fun propertySchema -> getEffectiveSchema propertySchema ctx)

        let getPatternPropertySchemas (instanceProperty : JProperty) : JsonSchemaElement list =
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

    and private tokenMatchesSchema (schema : JsonSchemaElement) (instance : JToken) (ctx : JsonContext) : seq<SchemaError> = seq {
        match (schema, matchJToken instance) with
        | JsonSchemaElement.Unvalidated, _                       -> yield! Seq.empty
        | JsonSchemaElement.Null,        MatchedJValueAsNull     -> yield! Seq.empty
        | JsonSchemaElement.Boolean,     MatchedJValueAsBool _   -> yield! Seq.empty
        | JsonSchemaElement.Number,      MatchedJValueAsInt _    -> yield! Seq.empty
        | JsonSchemaElement.String s,    MatchedJValueAsString i -> yield! stringMatchesSchema s i ctx
        | JsonSchemaElement.Array _,     MatchedJArray _         -> yield! Seq.empty
        | JsonSchemaElement.Object s,    MatchedJObject i        -> yield! objectMatchesSchema s i ctx
        | _,                        _                             -> yield reportTypeMismatch schema instance ctx }

    let validate (schema : JsonSchemaElement) (instance : JToken) : List<SchemaError> =
        {   SchemaRoot = schema
            InstanceRoot = instance
            CurrentPath = JsonPath [] }
        |> tokenMatchesSchema schema instance
        |> Seq.toList