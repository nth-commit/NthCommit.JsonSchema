namespace NthCommit.JsonSchema.Driver

open NthCommit.JsonSchema.Domain

[<RequireQualifiedAccess>]
type JsonElementInstance =
    | Null
    | Boolean of bool
    | Integer of int
    | String of string
    | Array of JsonDriverElement list
    | Object of JsonObjectInstance
    | Unhandled

and JsonPropertyInstance = {
    Name : string
    Value : JsonDriverElement }

and JsonObjectInstance =
    | JsonObjectInstance of JsonPropertyInstance list

    member this.Properties =
        let (JsonObjectInstance properties) = this
        properties

    member this.PropertyNames =
        this.Properties
        |> List.map (fun p -> p.Name)
        |> Set

    member this.TryFindProperty name =
        this.Properties
        |> List.tryFind (fun p -> p.Name = name)

and JsonDriverElement =
    abstract member Primitive: JsonPrimitive
    abstract member Match: unit -> JsonElementInstance

type JsonDeserializationError = {
    LineNumber: int
    LinePosition: int
    Path: string }

type JsonDriver =
    abstract member GetElement: string -> Result<JsonDriverElement, JsonDeserializationError>
