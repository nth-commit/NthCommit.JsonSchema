namespace NthCommit.JsonSchema.Parsing

type InvalidPropertyNameData = {
    Path : string
    Trivia : string }

type InvalidPropertyTypeData = {
    Path : string
    AcceptedTypes : string list }

[<RequireQualifiedAccess>]
type ParserError =
    | Json
    | PropertyName of InvalidPropertyNameData
    | PropertyType of InvalidPropertyTypeData
    | PropertyValue of string

module Errors =

    let signalPropertyName path trivia =
        { Path = path; Trivia = trivia }
        |> ParserError.PropertyName
        |> Error

    let signalPropertyType path acceptedTypes =
        { Path = path; AcceptedTypes = acceptedTypes }
        |> ParserError.PropertyType
        |> Error

    let signalOnePropertyType path acceptedType =
        signalPropertyType path [acceptedType]

    let signalPropertyValue path =
        path
        |> ParserError.PropertyValue
        |> Error
