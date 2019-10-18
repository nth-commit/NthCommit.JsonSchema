namespace NthCommit.JsonSchema.Parsing

open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver

module Arrays =

    let private parseItems
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (objectInstance : JsonObjectInstance) =
            objectInstance.TryFindProperty "items"
            |> Option.map (fun items -> parseSchemaToken items.Value)
            |> Option.defaultValue JsonElementSchema.Unvalidated

    let parse
        (parseSchemaToken : JsonDriverElement -> JsonElementSchema)
        (objectInstance : JsonObjectInstance) =
            JsonElementSchema.Array <| {
                Items = parseItems parseSchemaToken objectInstance }