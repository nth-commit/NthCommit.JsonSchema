namespace NthCommit.JsonSchema.Validation

open NthCommit.JsonSchema.Domain
open NthCommit.JsonSchema.Driver

module Arrays =

    let validate
        (validateElement : JsonElementSchema -> JsonDriverElement -> JsonContextReader<seq<SchemaError>>)
        (arraySchema : JsonArraySchema)
        (arrayInstance : JsonDriverElement list) =
            arrayInstance
            |> List.map (validateElement arraySchema.Items)
            |> JsonContextReader.concatSchemaErrors
