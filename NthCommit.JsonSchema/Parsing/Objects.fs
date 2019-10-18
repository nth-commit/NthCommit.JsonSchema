namespace NthCommit.JsonSchema.Parsing

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom
open NthCommit.JsonSchema.JsonHelper

module Arrays =

    let private parseItems
        (parseSchemaToken : JToken -> JsonElementSchema)
        (propertiesByName : Map<string, JsonPropertyInstance>) =
            propertiesByName
            |> Map.tryFind "items"
            |> Option.map (fun items -> parseSchemaToken items.Value)
            |> Option.defaultValue JsonElementSchema.Unvalidated

    let parse
        (parseSchemaToken : JToken -> JsonElementSchema)
        (propertiesByName : Map<string, JsonPropertyInstance>) =
            JsonElementSchema.Array <| {
                Items = parseItems parseSchemaToken propertiesByName }