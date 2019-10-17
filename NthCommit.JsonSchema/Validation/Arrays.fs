namespace NthCommit.JsonSchema.Validation

open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom

module Arrays =

    let validate
        (validateElement : JsonElementSchema -> JToken -> JsonContextReader<seq<SchemaError>>)
        (arraySchema : JsonArraySchema)
        (arrayInstance : JToken list) =
            arrayInstance
            |> List.map (validateElement arraySchema.Items)
            |> JsonContextReader.concatSchemaErrors
