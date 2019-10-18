namespace NthCommit.JsonSchema.Driver

open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Domain

type NewtonsoftJsonDriverElement(token : JToken) =
    interface JsonDriverElement with

        member _.Primitive =
            match token.Type with
            | JTokenType.Null       -> JsonPrimitive.Null
            | JTokenType.Boolean    -> JsonPrimitive.Boolean
            | JTokenType.Integer    -> JsonPrimitive.Number
            | JTokenType.String     -> JsonPrimitive.String
            | JTokenType.Array      -> JsonPrimitive.Array
            | JTokenType.Object     -> JsonPrimitive.Object
            | x                     -> raise (Exception("Unhandled JTokenType: " + x.ToString()))

        member _.Match () =
            match token.Type with
            | JTokenType.Null ->
                JsonElementInstance.Null
            | JTokenType.Boolean ->
                token.Value<bool>()
                |> JsonElementInstance.Boolean
            | JTokenType.Integer ->
                JsonElementInstance.Integer 0
            | JTokenType.String ->
                token.Value<string>()
                |> JsonElementInstance.String
            | JTokenType.Array ->
                (token :?> JArray).AsJEnumerable()
                |> Seq.map (fun t -> NewtonsoftJsonDriverElement(t) :> JsonDriverElement)
                |> Seq.toList
                |> JsonElementInstance.Array
            | JTokenType.Object ->
                (token :?> JObject).Properties()
                |> Seq.map (fun p -> {
                    Name = p.Name
                    Value = NewtonsoftJsonDriverElement(p.Value) :> JsonDriverElement })
                |> Seq.toList
                |> JsonObjectInstance
                |> JsonElementInstance.Object
            | _ -> JsonElementInstance.Unhandled

type NewtonsoftJsonDriver() =
    interface JsonDriver with
        member _.GetElement (json : string) =
            try
                JsonConvert.DeserializeObject<JToken>(json)
                |> NewtonsoftJsonDriverElement
                :> JsonDriverElement
                |> Ok
            with | :? JsonReaderException as ex -> Error {
                LineNumber = ex.LineNumber
                LinePosition = ex.LinePosition
                Path = ex.Path }
