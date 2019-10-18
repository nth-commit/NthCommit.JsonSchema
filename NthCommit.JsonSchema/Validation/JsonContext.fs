namespace NthCommit.JsonSchema.Validation

open System
open Newtonsoft.Json.Linq
open NthCommit.JsonSchema.Dom

type JsonPathComponent =
    | PropertyAccess of string
    | ArrayAccess of int

type JsonPath =
    | JsonPath of JsonPathComponent list

    member this.Push (newComponent : JsonPathComponent) =
        let (JsonPath components) = this
        components
        |> List.append [newComponent]
        |> JsonPath

    member this.Render () =
        let (JsonPath components) = this
        match components with
        | []    -> ""
        | _     ->
            components
            |> List.rev
            |> List.map (fun c ->
                match c with
                | PropertyAccess p  -> p
                | ArrayAccess _     -> raise (Exception ("Unhandled: ArrayAccess when rendering component")))
            |> List.reduce (sprintf "%s.%s")

type JsonContext =
    {   SchemaRoot : JsonElementSchema
        InstanceRoot : JToken
        CurrentPath : JsonPath }
        member this.PushProperty property =
            { this with CurrentPath = this.CurrentPath.Push (PropertyAccess property) }

type JsonContextReader<'a> = | JsonContextReader of (JsonContext -> 'a)

module JsonContextReader =

    let run (ctx : JsonContext) (JsonContextReader read) : 'a =
        read ctx

    let bind (f : 'a -> JsonContextReader<'b>) (reader : JsonContextReader<'a>) : JsonContextReader<'b> =
        JsonContextReader <| fun ctx ->
            let x = run ctx reader
            run ctx (f x)

    let retn x = JsonContextReader <| fun _ -> x

    let map (f : 'a -> 'b) (reader : JsonContextReader<'a>) : JsonContextReader<'b> = 
        JsonContextReader <| fun ctx ->
            let x = run ctx reader
            f x

    let local (f : JsonContext -> JsonContext) (JsonContextReader read) : JsonContextReader<'a> =
        JsonContextReader (f >> read)

    let rec concat (readers : seq<JsonContextReader<'a>>) : JsonContextReader<seq<'a>> =
        if readers |> Seq.isEmpty
        then retn Seq.empty
        else
            readers |> Seq.head |> bind (fun x ->
            readers |> Seq.tail |> concat |> map (fun xs -> Seq.concat [Seq.singleton x; xs]))

    let concatSchemaErrors (errorReaders : seq<JsonContextReader<seq<SchemaError>>>) =
        errorReaders
        |> concat
        |> map Seq.concat

