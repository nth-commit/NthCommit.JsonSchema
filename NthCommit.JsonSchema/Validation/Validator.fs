namespace NthCommit.JsonSchema

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper
open System.Text.RegularExpressions

type SchemaTypeError = {
    Path            : string
    ExpectedTypes   : Set<JsonPrimitive>
    ActualType      : JsonPrimitive }

type SchemaValueError = {
    Path    : string
    Value   : string }

type SchemaRequiredError = {
    Path : string
    PropertyName : string }

[<RequireQualifiedAccess>]
type SchemaError =
    | Json of string
    | Type of SchemaTypeError
    | Value of SchemaValueError
    | Required of SchemaRequiredError

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
        {   SchemaRoot : JsonElementSchema
            InstanceRoot : JToken
            CurrentPath : JsonPath }
            member this.PushProperty property =
                { this with CurrentPath = this.CurrentPath.Push (PropertyAccess property) }

    let private reportTypeMismatch (schema : JsonElementSchema) (instance : JToken) (ctx : JsonContext) =
        SchemaError.Type {
            Path            = ctx.CurrentPath.Render()
            ExpectedTypes   = Set([schema.Primitive])
            ActualType      = instance.Type |> JTokenType.toPrimitive }

    let private validateInstanceIsValue (instance : 'a) (value : 'a) (ctx : JsonContext) = seq {
        if value <> instance
        then yield SchemaError.Value {
            Path = ctx.CurrentPath.Render()
            Value = instance.ToString() } }

    let private validateInstanceInValues (instance : 'a) (values : Set<'a>) (ctx : JsonContext) = seq {
        if values |> Set.contains instance |> not
        then yield SchemaError.Value {
            Path = ctx.CurrentPath.Render()
            Value = instance.ToString() } }

    let private stringMatchesSchema (schema : JsonStringSchema) (instance : string) (ctx : JsonContext) : seq<SchemaError> =
        match schema with
        | JsonStringSchema.Const value -> validateInstanceIsValue instance value ctx
        | JsonStringSchema.Enum values -> validateInstanceInValues instance values ctx
        | JsonStringSchema.Unvalidated -> Seq.empty

    let private evaluateReference (JsonReference reference) (ctx : JsonContext) : JsonElementSchema =
        match reference with
        | "#"   -> ctx.SchemaRoot
        | x     -> raise (NotImplementedException ("Reference is not supported: " + x))

    let private getPropertySchema (propertySchema : JsonPropertySchema) (ctx : JsonContext) : JsonElementSchema =
        match propertySchema with
        | Inline (_, schema) -> schema
        | Reference reference -> evaluateReference reference ctx

    let private validateRequiredProperties (objectSchema : JsonObjectSchema) (objectInstance : JProperty list) (ctx : JsonContext) : seq<SchemaError> =
        let instancePropertyNames =
            objectInstance
            |> List.map (fun p -> p.Name)
            |> Set
        Set.difference objectSchema.Required instancePropertyNames
        |> Seq.map (fun x ->
            {   Path = ctx.CurrentPath.Render()
                PropertyName = x }
            |> SchemaError.Required)

    let rec private validateProperty (schemas : JsonElementSchema list) (instance : JProperty) (ctx : JsonContext) : seq<SchemaError> =
        schemas
        |> List.map (fun s -> elementMatchesSchema s instance.Value ctx)
        |> Seq.concat

    and private objectMatchesSchema (objectSchema : JsonObjectSchema) (objectInstance : JProperty list) (ctx : JsonContext) : seq<SchemaError> = seq {
        let schemaPropertiesByName =
            objectSchema.Properties
            |> List.toMap (fun p -> p.Name)

        let getPropertySchemaByName (propertyInstance : JProperty) : JsonElementSchema option =
            schemaPropertiesByName
            |> Map.tryFind (propertyInstance.Name)
            |> Option.map (fun propertySchema -> getPropertySchema propertySchema ctx)

        let getPropertySchemasByPattern (propertyInstance : JProperty) : JsonElementSchema list =
            objectSchema.PatternProperties
            |> List.filter (fun ((RegularExpression pattern), _) -> Regex(pattern).IsMatch(propertyInstance.Name))
            |> List.map (fun (_, s) -> getPropertySchema (s |> List.head) ctx) // TODO: List.head...

        let getPropertySchemas (propertyInstance : JProperty) =
            List.concat [
                getPropertySchemaByName propertyInstance |> Option.toList
                getPropertySchemasByPattern propertyInstance ]

        yield!
            objectInstance
            |> List.map (fun p -> (getPropertySchemas p, p))
            |> List.map (fun (propertySchemas, propertyInstance) ->
                validateProperty propertySchemas propertyInstance (ctx.PushProperty propertyInstance.Name))
            |> Seq.concat

        yield!
            validateRequiredProperties objectSchema objectInstance ctx }

    and private elementMatchesSchema (schema : JsonElementSchema) (instance : JToken) (ctx : JsonContext) : seq<SchemaError> = seq {
        match (schema, matchJToken instance) with
        | JsonElementSchema.Unvalidated, _                       -> yield! Seq.empty
        | JsonElementSchema.Null,        MatchedJValueAsNull     -> yield! Seq.empty
        | JsonElementSchema.Boolean,     MatchedJValueAsBool _   -> yield! Seq.empty
        | JsonElementSchema.Number,      MatchedJValueAsInt _    -> yield! Seq.empty
        | JsonElementSchema.String s,    MatchedJValueAsString i -> yield! stringMatchesSchema s i ctx
        | JsonElementSchema.Array _,     MatchedJArray _         -> yield! Seq.empty
        | JsonElementSchema.Object s,    MatchedJObject i        -> yield! objectMatchesSchema s i ctx
        | _,                        _                             -> yield reportTypeMismatch schema instance ctx }

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
            match elementMatchesSchema schema instance ctx |> Seq.toList with
            | [] -> Ok instance
            | list -> Error list
        | Error e -> Error [e]