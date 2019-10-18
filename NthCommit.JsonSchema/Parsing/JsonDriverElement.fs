namespace NthCommit.JsonSchema.Parsing

open System
open NthCommit.JsonSchema.Driver

module JsonDriverElement =

    let private matchElement (element : JsonDriverElement) =
        element.Match()

    let private throwUnexpectedToken () =
        raise (Exception ("Unexpected token"))

    let asBoolean element =
        matchElement element
        |> function 
            | JsonElementInstance.Boolean instance -> instance
            | _ -> throwUnexpectedToken ()

    let getInteger element =
        matchElement element
        |> function 
            | JsonElementInstance.Integer instance -> instance
            | _ -> throwUnexpectedToken ()

    let getString element =
        matchElement element
        |> function 
            | JsonElementInstance.String instance -> instance
            | _ -> throwUnexpectedToken ()

    let getArray element =
        matchElement element
        |> function 
            | JsonElementInstance.Array instance -> instance
            | _ -> throwUnexpectedToken ()

    let getObject element =
        matchElement element
        |> function 
            | JsonElementInstance.Object instance -> instance
            | _ -> throwUnexpectedToken ()

    let getObjectProperties element =
        (getObject element).Properties
