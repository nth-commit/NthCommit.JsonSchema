namespace NthCommit.JsonSchema

type JsonObjectSchema = {
    Properties : (string * JsonSchema) list
    RequiredProperies : string list
    AdditionalProperties : bool }

and JsonArraySchema = {
    Items : JsonSchema }

and JsonPrimitiveSchema =
    | JsonPrimitiveString

and JsonSchema =
    | Object of JsonObjectSchema
    | Array of JsonSchema
    | Primitive of JsonPrimitiveSchema
    | Unvalidated
