namespace NthCommit.JsonSchema.Parsing

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper

module Strings =

    module private Option =

        let bindOr (f : unit -> 'a option) x =
            match x with
            | Some x' -> Some x'
            | None ->
                match f () with
                | Some x' -> Some x'
                | None -> None

    let tryParseEnum (propertiesByName : Map<string, JsonPropertyInstance>) =
        propertiesByName |> Map.tryFind "enum"
        |> Option.map (fun enumProperty ->
            (enumProperty.Value :?> JArray)
            |> Seq.map (fun enumValueToken -> enumValueToken.Value<string>())
            |> Set
            |> JsonStringSchema.Enum) : Option<JsonStringSchema>

    let tryParseConst (propertiesByName : Map<string, JsonPropertyInstance>) =
        propertiesByName |> Map.tryFind "const"
        |> Option.map (fun constProperty ->
            constProperty.Value.Value<string>()
            |> JsonStringSchema.Const) : Option<JsonStringSchema>

    let parse (propertiesByName : Map<string, JsonPropertyInstance>) =
        tryParseEnum propertiesByName
        |> Option.bindOr (fun _ -> tryParseConst propertiesByName)
        |> Option.defaultValue JsonStringSchema.Unvalidated
        |> JsonElementSchema.String

