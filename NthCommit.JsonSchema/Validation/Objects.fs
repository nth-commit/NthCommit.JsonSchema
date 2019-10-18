namespace NthCommit.JsonSchema.Validation

open System
open System.Text.RegularExpressions
open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver

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

    let private validateMany
        (validateElement : JsonElementSchema -> JsonDriverElement -> JsonContextReader<seq<SchemaError>>)
        (schemas : JsonElementSchema seq)
        (instance : JsonDriverElement) =
            schemas
            |> Seq.map (fun s -> validateElement s instance)
            |> JsonContextReader.concatSchemaErrors

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
            (validateElement : JsonElementSchema -> JsonDriverElement -> JsonContextReader<seq<SchemaError>>)
            (objectSchema : JsonObjectSchema)
            (propertyInstance : JsonPropertyInstance) =
                getPropertySchemas objectSchema propertyInstance.Name
                |> JsonContextReader.bind (fun propertySchemas ->
                    validateMany validateElement propertySchemas propertyInstance.Value)

        let validateProperties
            (validateElement : JsonElementSchema -> JsonDriverElement -> JsonContextReader<seq<SchemaError>>)
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

    let validate
        (validateElement : JsonElementSchema -> JsonDriverElement -> JsonContextReader<seq<SchemaError>>)
        (objectSchema : JsonObjectSchema)
        (objectInstance : JsonObjectInstance) =
            [   Properties.validateProperties validateElement
                Required.validateRequiredProperties
                AdditionalProperties.validateAdditionalProperties ]
            |> List.map (fun f -> f objectSchema objectInstance)
            |> JsonContextReader.concat
            |> JsonContextReader.map Seq.concat