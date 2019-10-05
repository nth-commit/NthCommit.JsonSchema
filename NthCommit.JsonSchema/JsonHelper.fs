namespace NthCommit.JsonSchema

module JsonHelper =

    open System
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    let tryDeserialize<'a> (json : string) : Result<'a, unit> =
        try JsonConvert.DeserializeObject<'a>(json) |> Ok
        with | :? JsonException -> Error ()

    type JsonToken =
        | JsonObject of JProperty list
        | JsonString of string
        | JsonProperty of string * JToken
        | Unhandled

    let matchJToken (jt : JToken) =
        match jt.Type with
        | JTokenType.Object ->
            (jt :?> JObject).Properties() |> Seq.toList |> JsonObject
        | JTokenType.String ->
            JsonString (jt.Value<string>())
        | JTokenType.Property ->
            let jPropery = jt :?> JProperty
            JsonProperty (jPropery.Name, jPropery.Value)
        | _ ->
            Unhandled

    let unmatchJToken = function
        | JsonObject _ -> JTokenType.Object
        | JsonString _ -> JTokenType.String
        | _ -> raise (Exception "Unrecognized token")