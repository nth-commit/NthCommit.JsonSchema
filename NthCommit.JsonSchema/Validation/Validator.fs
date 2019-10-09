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
        {   SchemaRoot : JsonDocument
            InstanceRoot : JToken
            CurrentPath : JsonPath }
            member this.PushProperty property =
                { this with CurrentPath = this.CurrentPath.Push (PropertyAccess property) }

    let private reportTypeMismatch (schema : JsonDocument) (instance : JToken) (ctx : JsonContext) =
        SchemaError.Type {
            Path            = ctx.CurrentPath.Render()
            ExpectedTypes   = Set([schema.Primitive])
            ActualType      = instance.Type |> JTokenType.toPrimitive }

    let private validateInstanceInValues (instance : 'a) (values : 'a list) (ctx : JsonContext) = seq {
        if values |> List.contains instance |> not
        then yield SchemaError.Value {
            Path = ctx.CurrentPath.Render()
            Value = instance.ToString() } }

    let private stringMatchesSchema (schema : JsonString) (instance : string) (ctx : JsonContext) : seq<SchemaError> =
        match schema with
        | JsonString.Unvalidated  -> Seq.empty
        | JsonString.Enum values  -> validateInstanceInValues instance values ctx
        | JsonString.Const _      -> raise (Exception ("Unhandled: JsonStringSchema.Const"))

    let private evaluateReference (JsonReference reference) (ctx : JsonContext) : JsonDocument =
        match reference with
        | "#"   -> ctx.SchemaRoot
        | x     -> raise (NotImplementedException ("Reference is not supported: " + x))

    let private getEffectiveSchema (propertySchema : JsonObjectProperty) (ctx : JsonContext) : JsonDocument =
        match propertySchema with
        | Inline (_, schema)  -> schema
        | Reference reference   -> evaluateReference reference ctx

    let rec private validateProperty (schemas : JsonDocument list) (instance : JProperty) (ctx : JsonContext) : seq<SchemaError> =
        schemas
        |> List.map (fun s -> tokenMatchesSchema s instance.Value ctx)
        |> Seq.concat

    and private objectMatchesSchema (schema : JsonObject) (instance : JProperty list) (ctx : JsonContext) : seq<SchemaError> =
        let schemaPropertiesByName =
            schema.Properties
            |> List.toMap (fun p -> p.Name)

        let getSpecificPropertySchema (instanceProperty : JProperty) : JsonDocument option =
            schemaPropertiesByName
            |> Map.tryFind (instanceProperty.Name)
            |> Option.map (fun propertySchema -> getEffectiveSchema propertySchema ctx)

        let getPatternPropertySchemas (instanceProperty : JProperty) : JsonDocument list =
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

    and private tokenMatchesSchema (schema : JsonDocument) (instance : JToken) (ctx : JsonContext) : seq<SchemaError> = seq {
        match (schema, matchJToken instance) with
        | JsonDocument.Unvalidated, _                       -> yield! Seq.empty
        | JsonDocument.Null,        MatchedJValueAsNull     -> yield! Seq.empty
        | JsonDocument.Boolean,     MatchedJValueAsBool _   -> yield! Seq.empty
        | JsonDocument.Number,      MatchedJValueAsInt _    -> yield! Seq.empty
        | JsonDocument.String s,    MatchedJValueAsString i -> yield! stringMatchesSchema s i ctx
        | JsonDocument.Array _,     MatchedJArray _         -> yield! Seq.empty
        | JsonDocument.Object s,    MatchedJObject i        -> yield! objectMatchesSchema s i ctx
        | _,                        _                       -> yield reportTypeMismatch schema instance ctx }

    let validate (schema : JsonDocument) (instance : JToken) : List<SchemaError> =
        {   SchemaRoot = schema
            InstanceRoot = instance
            CurrentPath = JsonPath [] }
        |> tokenMatchesSchema schema instance
        |> Seq.toList