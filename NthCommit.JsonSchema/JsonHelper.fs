namespace NthCommit.JsonSchema

module JsonHelper =

    open System
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    let tryDeserialize<'a> (json : string) : Result<'a, unit> =
        try JsonConvert.DeserializeObject<'a>(json) |> Ok
        with | :? JsonException -> Error ()

    type MatchedJToken =
        | MatchedJObject of JProperty list
        | MatchedJValueAsString of string
        | Unhandled

    let matchJToken (jt : JToken) =
        match jt.Type with
        | JTokenType.Object ->
            (jt :?> JObject).Properties() |> Seq.toList |> MatchedJObject
        | JTokenType.String ->
            MatchedJValueAsString (jt.Value<string>())
        | _ ->
            Unhandled

    let unmatchJToken = function
        | MatchedJObject _ -> JTokenType.Object
        | MatchedJValueAsString _ -> JTokenType.String
        | _ -> raise (Exception "Unrecognized token")