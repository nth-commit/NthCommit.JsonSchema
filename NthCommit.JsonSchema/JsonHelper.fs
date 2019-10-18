namespace NthCommit.JsonSchema

module JsonHelper =

    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    type JsonDeserializationError = {
        LineNumber: int
        LinePosition: int
        Path: string }

    let tryDeserialize<'a> (json : string) : Result<'a, JsonDeserializationError> =
        try JsonConvert.DeserializeObject<'a>(json) |> Ok
        with | :? JsonReaderException as ex -> Error {
            LineNumber = ex.LineNumber
            LinePosition = ex.LinePosition
            Path = ex.Path }

    type JsonPropertyInstance = {
        Name : string
        Value : JToken }

    type JsonObjectInstance =
        | JsonObjectInstance of JProperty list

        member this.Properties =
            let (JsonObjectInstance properties) = this
            properties
            |> List.map (fun p -> {
                Name = p.Name
                Value = p.Value })

        member this.PropertyNames =
            this.Properties
            |> List.map (fun p -> p.Name)
            |> Set

    [<RequireQualifiedAccess>]
    type JsonElementInstance =
        | Null
        | Boolean of bool
        | Integer of int
        | String of string
        | Array of JToken list
        | Object of JsonObjectInstance
        | Unhandled

    let matchJToken (jt : JToken) =
        match jt.Type with
        | JTokenType.Null -> JsonElementInstance.Null
        | JTokenType.Boolean -> JsonElementInstance.Boolean true
        | JTokenType.Integer -> JsonElementInstance.Integer 0
        | JTokenType.String -> JsonElementInstance.String (jt.Value<string>())
        | JTokenType.Array -> (jt :?> JArray).AsJEnumerable() |> Seq.toList |> JsonElementInstance.Array
        | JTokenType.Object -> (jt :?> JObject).Properties() |> Seq.toList |> JsonObjectInstance |> JsonElementInstance.Object
        | _ -> JsonElementInstance.Unhandled

[<RequireQualifiedAccess>]
type JsonPrimitive =
    | Null
    | Boolean
    | Number
    | String
    | Array
    | Object
    member x.ToSchemaTypeValue() =
        match x with
        | Null      -> "null"
        | Boolean   -> "boolean"
        | Number    -> "number"
        | String    -> "string"
        | Array     -> "array"
        | Object    -> "object"
    override x.ToString() = x.ToSchemaTypeValue()