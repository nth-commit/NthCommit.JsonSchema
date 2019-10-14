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
    RequiredPropertyName : string }

type SchemaAdditionError = {
    Path : string
    AdditionalPropertyName : string }

[<RequireQualifiedAccess>]
type SchemaError =
    | Json of string
    | Type of SchemaTypeError
    | Value of SchemaValueError
    | Required of SchemaRequiredError
    | Additional of SchemaAdditionError

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

    type JsonContextReader<'a> = | JsonContextReader of (JsonContext -> 'a)

    module JsonContextReader =

        let run (ctx : JsonContext) (JsonContextReader read) : 'a =
            read ctx

        let bind (f : 'a -> JsonContextReader<'b>) (reader : JsonContextReader<'a>) : JsonContextReader<'b> =
            JsonContextReader <| fun ctx ->
                let x = run ctx reader
                run ctx (f x)

        let retn x = JsonContextReader <| fun _ -> x

        let map (f : 'a -> 'b) (reader : JsonContextReader<'a>) : JsonContextReader<'b> = 
            JsonContextReader <| fun ctx ->
                let x = run ctx reader
                f x

        let local (f : JsonContext -> JsonContext) (JsonContextReader read) : JsonContextReader<'a> =
            JsonContextReader (f >> read)

        let rec concat (readers : seq<JsonContextReader<'a>>) : JsonContextReader<seq<'a>> =
            if readers |> Seq.isEmpty
            then retn Seq.empty
            else
                readers |> Seq.head |> bind (fun x ->
                readers |> Seq.tail |> concat |> map (fun xs -> Seq.concat [Seq.singleton x; xs]))

        let concatSchemaErrors (errorReaders : seq<JsonContextReader<seq<SchemaError>>>) =
            errorReaders
            |> concat
            |> map Seq.concat

    let private validateMany
        (validateElement : JsonElementSchema -> JToken -> JsonContextReader<seq<SchemaError>>)
        (schemas : JsonElementSchema seq)
        (instance : JToken) =
            schemas
            |> Seq.map (fun s -> validateElement s instance)
            |> JsonContextReader.concatSchemaErrors

    let private reportTypeMismatch
        (schema : JsonElementSchema)
        (instance : JToken) =
            JsonContextReader <| fun ctx -> seq {
                SchemaError.Type {
                    Path = ctx.CurrentPath.Render()
                    ExpectedTypes = Set([schema.Primitive])
                    ActualType = instance.Type |> JTokenType.toPrimitive } }

    module private Strings =

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

        let validateString (schema : JsonStringSchema) (instance : string) =
            match schema with
            | JsonStringSchema.Const value -> validateInstanceIsValue value instance
            | JsonStringSchema.Enum values -> validateInstanceInValues values instance
            | JsonStringSchema.Unvalidated -> JsonContextReader.retn Seq.empty

    module Arrays =

        let validateArray
            (validateElement : JsonElementSchema -> JToken -> JsonContextReader<seq<SchemaError>>)
            (arraySchema : JsonArraySchema)
            (arrayInstance : JToken list) =
                arrayInstance
                |> List.map (validateElement arraySchema.Items)
                |> JsonContextReader.concatSchemaErrors

    module Objects =

        let private evaluateReference (JsonReference reference) =
            JsonContextReader <| fun ctx ->
                match reference with
                | "#" -> ctx.SchemaRoot
                | x -> raise (NotImplementedException ("Reference is not supported: " + x))

        let private getPropertySchema (propertySchema : JsonPropertySchema) = 
            match propertySchema with
            | Inline (_, schema) -> JsonContextReader.retn schema
            | Reference reference -> evaluateReference reference

        module Properties =

            let private getPropertySchemaByName
                (propertySchemas : JsonPropertySchema list)
                (propertyName : string) =
                    propertySchemas
                    |> List.tryFind (fun p -> p.Name = propertyName)
                    |> Option.map getPropertySchema

            let private getPropertySchemasByPattern
                (patternProperties : (RegularExpression * JsonPropertySchema list) list)
                (propertyName : string) =
                    patternProperties
                    |> List.filter (fun ((RegularExpression pattern), _) -> Regex(pattern).IsMatch(propertyName))
                    |> List.map (fun (_, s) -> getPropertySchema (s |> List.head)) // TODO: List.head...

            let private getPropertySchemas
                (objectSchema : JsonObjectSchema)
                (propertyName : string) =
                    List.concat [
                        getPropertySchemaByName objectSchema.Properties propertyName |> Option.toList
                        getPropertySchemasByPattern objectSchema.PatternProperties propertyName ]
                    |> JsonContextReader.concat

            let private validateProperty
                (validateElement : JsonElementSchema -> JToken -> JsonContextReader<seq<SchemaError>>)
                (objectSchema : JsonObjectSchema)
                (propertyInstance : JsonPropertyInstance) =
                    getPropertySchemas objectSchema propertyInstance.Name
                    |> JsonContextReader.bind (fun propertySchemas ->
                        validateMany validateElement propertySchemas propertyInstance.Value)

            let validateProperties
                (validateElement : JsonElementSchema -> JToken -> JsonContextReader<seq<SchemaError>>)
                (objectSchema : JsonObjectSchema)
                (objectInstance : JsonObjectInstance) =
                    objectInstance.Properties
                    |> List.map (fun propertyInstance ->
                        JsonContextReader.local
                            (fun ctx -> ctx.PushProperty propertyInstance.Name)
                            (validateProperty validateElement objectSchema propertyInstance))
                    |> JsonContextReader.concatSchemaErrors

        module private Required =

            let private reportRequiredPropertyMissing (propertyName : string) =
                JsonContextReader <| fun ctx -> seq {
                    SchemaError.Required {
                        Path = ctx.CurrentPath.Render()
                        RequiredPropertyName = propertyName } }

            let validateRequiredProperties
                (objectSchema : JsonObjectSchema)
                (objectInstance : JsonObjectInstance) =
                    Set.difference objectSchema.Required objectInstance.PropertyNames
                    |> Seq.map reportRequiredPropertyMissing
                    |> JsonContextReader.concatSchemaErrors

        module private AdditionalProperties =

            let private reportAdditionalPropertyPresent (propertyName : string) =
                JsonContextReader <| fun ctx -> seq {
                    SchemaError.Additional { 
                        Path = ctx.CurrentPath.Render()
                        AdditionalPropertyName = propertyName } }

            let validateAdditionalProperties
                (objectSchema : JsonObjectSchema)
                (objectInstance : JsonObjectInstance) =
                    if objectSchema.AdditionalProperties
                    then JsonContextReader.retn Seq.empty
                    else
                        Set.difference objectInstance.PropertyNames objectSchema.PropertyNames
                        |> Seq.map reportAdditionalPropertyPresent
                        |> JsonContextReader.concatSchemaErrors

        let validateObject
            (validateElement : JsonElementSchema -> JToken -> JsonContextReader<seq<SchemaError>>)
            (objectSchema : JsonObjectSchema)
            (objectInstance : JsonObjectInstance) =
                [   Properties.validateProperties validateElement
                    Required.validateRequiredProperties
                    AdditionalProperties.validateAdditionalProperties ]
                |> List.map (fun validate -> validate objectSchema objectInstance)
                |> JsonContextReader.concat
                |> JsonContextReader.map Seq.concat

    let rec private validateElement (schema : JsonElementSchema) (instance : JToken) =
        match (schema, matchJToken instance) with
        | JsonElementSchema.Null, JsonElementInstance.Null -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.Boolean, JsonElementInstance.Boolean _ -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.Number, JsonElementInstance.Integer _ -> JsonContextReader.retn Seq.empty
        | JsonElementSchema.String s, JsonElementInstance.String i -> Strings.validateString s i
        | JsonElementSchema.Array s, JsonElementInstance.Array i -> Arrays.validateArray validateElement s i
        | JsonElementSchema.Object s, JsonElementInstance.Object i -> Objects.validateObject validateElement s i
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