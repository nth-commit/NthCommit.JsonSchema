namespace NthCommit.JsonSchema

module JsonHelper =

    open System
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    let tryDeserialize<'a> (json : string) : Result<'a, unit> =
        try JsonConvert.DeserializeObject<'a>(json) |> Ok
        with | :? JsonException -> Error ()

    type MatchedJToken =
        | MatchedJValueAsNull
        | MatchedJValueAsBool   of bool
        | MatchedJValueAsInt    of int
        | MatchedJValueAsString of string
        | MatchedJArray         of JToken list
        | MatchedJObject        of JProperty list
        | Unhandled

    let matchJToken (jt : JToken) =
        match jt.Type with
        | JTokenType.Null       -> MatchedJValueAsNull
        | JTokenType.Boolean    -> MatchedJValueAsBool true
        | JTokenType.Integer    -> MatchedJValueAsInt 0
        | JTokenType.String     -> MatchedJValueAsString (jt.Value<string>())
        | JTokenType.Array      -> (jt :?> JArray).AsJEnumerable() |> Seq.toList |> MatchedJArray
        | JTokenType.Object     -> (jt :?> JObject).Properties() |> Seq.toList |> MatchedJObject
        | _ ->
            Unhandled

    let unmatchJToken = function
        | MatchedJObject _ -> JTokenType.Object
        | MatchedJValueAsString _ -> JTokenType.String
        | _ -> raise (Exception "Unrecognized token")

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