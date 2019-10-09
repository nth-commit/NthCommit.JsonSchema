﻿module Gen

open Hedgehog
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema
open System

module private Result =

    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    let isError r = not <| isOk r

module Strings =

    module private String =

        let capitalize (str : string) =
            match (str |> Seq.toList) with
            | [] -> ""
            | x :: xs ->
                x.ToString().ToUpper() :: (xs |> List.map string)
                |> List.reduce (+)

    let lowerCaseWord =
        ['a' .. 'z']
        |> Gen.item
        |> Gen.string (Range.exponential 1 100)

    let camelCaseWord =
        lowerCaseWord
        |> Gen.list (Range.linear 1 10)
        |> Gen.map (function
            | [] -> ""
            | x :: xs ->
                x :: (xs |> List.map String.capitalize)
                |> List.reduce (+))

    let defaultString = camelCaseWord

    let whitespace = gen {
        let whitespaceChar = [' '] |> Gen.item
        return! Gen.string (Range.exponential 0 1000) whitespaceChar }

module Json =

    let private tryDeserialize (json : string) : Result<'a, string> =
        try JsonConvert.DeserializeObject<'a>(json) |> Ok
        with | :? JsonException -> Error "Invalid JSON"
    
    let invalid =
        Gen.constant "not json"
        |> Gen.filter (tryDeserialize >> Result.isError)
    
    let valid =
        Gen.constant "{}"
        |> Gen.filter (tryDeserialize >> Result.isOk)

    let value : Gen<JValue> = [
        JValue.CreateNull() |> Gen.constant
        Gen.integral (Range.exponential -1000 1000) |> Gen.map (fun i -> JValue(i))
        Gen.bool |> Gen.map (fun b -> JValue(b))
        Strings.camelCaseWord |> Gen.map (fun s -> JValue(s)) ] |> Gen.choice

    let token = [
        value |> Gen.map (fun v -> v :> JToken)
        JArray.FromObject([]) |> Gen.constant |> Gen.map (fun a -> a :> JToken)
        JObject.FromObject({||}) |> Gen.constant |> Gen.map (fun o -> o :> JToken) ] |> Gen.choice

    let tokenOf (kind : JTokenType) =
        token
        |> Gen.filter (fun t -> t.Type = kind)

    let tokenNotOf (kind : JTokenType) =
        token
        |> Gen.filter (fun t -> t.Type <> kind)

    let stringValueNotOf strings =
        tokenOf JTokenType.String
        |> Gen.filter (fun t ->
            List.contains (t.Value<string>()) strings
            |> not)

    let propertyWithValue (name : Gen<string>) (value : Gen<JToken>) : Gen<JProperty> =
        Gen.zip name value
        |> Gen.map (fun (n, v) -> JProperty(n, v))

    let property name = propertyWithValue name token

    let objectOfOneProperty (property : Gen<JProperty>) : Gen<JObject> =
        property
        |> Gen.map (fun p -> JObject([p]))

    let serialize token =
        token
        |> Gen.map JsonConvert.SerializeObject

    let maybeAdditive (json : string) = gen {
        let jObject = JsonConvert.DeserializeObject<JObject> json
        let! newProperties =
            property Strings.camelCaseWord
            |> Gen.list (Range.exponential 0 50)
        let oldProperties = jObject.Properties() |> Seq.toList
        let mergedProperties =
            oldProperties @ newProperties
            |> List.distinctBy (fun p -> p.Name)
        return JsonConvert.SerializeObject(JObject (mergedProperties)) }

    let primitive =
        Gen.item [
            JsonPrimitive.Null
            JsonPrimitive.Boolean
            JsonPrimitive.Number
            JsonPrimitive.String
            JsonPrimitive.Array
            JsonPrimitive.Object ]

    let jsonOfPrimitive primitive =
        match primitive with
        | JsonPrimitive.Null    -> JTokenType.Null
        | JsonPrimitive.Boolean -> JTokenType.Boolean
        | JsonPrimitive.Number  -> JTokenType.Integer
        | JsonPrimitive.String  -> JTokenType.String
        | JsonPrimitive.Array   -> JTokenType.Array
        | JsonPrimitive.Object  -> JTokenType.Object
        |> tokenOf
        |> serialize

    let jsonNotOfPrimitive primitive =
        match primitive with
        | JsonPrimitive.Null    -> JTokenType.Null
        | JsonPrimitive.Boolean -> JTokenType.Boolean
        | JsonPrimitive.Number  -> JTokenType.Integer
        | JsonPrimitive.String  -> JTokenType.String
        | JsonPrimitive.Array   -> JTokenType.Array
        | JsonPrimitive.Object  -> JTokenType.Object
        |> tokenNotOf
        |> serialize

let notIn source generator =
    generator
    |> Gen.filter (fun x -> List.contains x source |> not)