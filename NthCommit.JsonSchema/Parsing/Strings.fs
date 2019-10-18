namespace NthCommit.JsonSchema.Parsing

open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver

module Strings =

    module private Option =
        let bindOr (f : unit -> 'a option) x =
            match x with
            | Some x' -> Some x'
            | None ->
                match f () with
                | Some x' -> Some x'
                | None -> None

    let tryParseEnum (objectInstance : JsonObjectInstance) =
        objectInstance.TryFindProperty "enum"
        |> Option.map (fun enumProperty ->
            enumProperty.Value
            |> JsonDriverElement.getArray
            |> List.map JsonDriverElement.getString
            |> (Set >> JsonStringSchema.Enum)) : Option<JsonStringSchema>

    let tryParseConst (objectInstance : JsonObjectInstance) =
        objectInstance.TryFindProperty "const"
        |> Option.map (fun constProperty ->
            constProperty.Value
            |> JsonDriverElement.getString
            |> JsonStringSchema.Const) : Option<JsonStringSchema>

    let parse (objectInstance : JsonObjectInstance) =
        tryParseEnum objectInstance
        |> Option.bindOr (fun _ -> tryParseConst objectInstance)
        |> Option.defaultValue JsonStringSchema.Unvalidated
        |> JsonElementSchema.String
