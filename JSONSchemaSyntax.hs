module JSONSchemaSyntax where

import Numeric.Natural
import Data.List (intercalate)

data JSONDocumentSchema = JSONDocument JSONSchema
                        | JSONDocumentDef [(String, JSONSchema)] JSONSchema
                        deriving Show

data JSONSchema = JSONNullSchema
                | JSONBoolSchema
                | JSONStringSchema [JSONStringRes]
                | JSONNumberSchema [JSONNumberRes]
                | JSONIntegerSchema [JSONNumberRes]
                | JSONObjectSchema [JSONObjectRes]
                | JSONArraySchema [JSONArrayRes]
                | JSONNotSchema JSONSchema
                | JSONAllOfSchema [JSONSchema] -- non-empty
                | JSONAnyOfSchema [JSONSchema] -- non-empty
--                | Enum [JValue] -- non-empty
--                | Ref JPointer 
--                deriving Show

data JSONStringRes = MinLength Integer
                   | MaxLength Integer
--                   | Pattern RegExp
--                   deriving Show

data JSONNumberRes = Min Integer
                   | ExMin Integer
                   | Max Integer
                   | ExMax Integer
                   | Mult Integer
--                   deriving Show

data JSONObjectRes = Prop [(String, JSONSchema)] -- non-empty
                   | AddProp Bool
                   | Required [String] -- non-empty
                   | MinProp Integer
                   | MaxProp Integer
--                   | PatProp [(RegExp, JSONSchema)] -- non-empty
--                   deriving Show

data JSONArrayRes = Items JSONSchema -- List validation
                  | PrefixItems [JSONSchema] -- Tuple validation
                  | MinItems Integer
                  | MaxItems Integer
                  | UniqueItems Bool
--                  deriving Show

data RegExp = Anything
            deriving Show

showProp :: (String, JSONSchema) -> String
showProp (k,v) = "\"" ++ k ++ "\" : { " ++ show v ++ " }"

instance Show JSONSchema where
    show value = case value of 
        JSONNullSchema      -> "\"type\" : \"null\""
        JSONBoolSchema      -> "\"type\" : \"boolean\""
        JSONStringSchema l  -> "\"type\" : \"string\", " ++ intercalate ", " (map show l)
        JSONNumberSchema l  -> "\"type\" : \"number\", " ++ intercalate ", " (map show l)
        JSONIntegerSchema l -> "\"type\" : \"integer\", " ++ intercalate ", " (map show l)
        JSONObjectSchema l  -> "\"type\" : \"object\",\n" ++ intercalate ", " (map show l)
        JSONArraySchema l   -> "\"type\" : \"array\", \n" ++ intercalate ", " (map show l)
        JSONNotSchema s     -> "\"not\" : { " ++ show s ++ " }"
        JSONAllOfSchema l   -> "\"allOf\" : [\n  { " ++ intercalate " },\n  { " (map show l) ++ " }\n]"
        JSONAnyOfSchema l   -> "\"anyOf\" : [\n  { " ++ intercalate " },\n  { " (map show l) ++ " }\n]"

instance Show JSONStringRes where 
    show value = case value of 
        MinLength n -> "\"minLength\" : " ++ show n
        MaxLength n -> "\"maxLength\" : " ++ show n

instance Show JSONNumberRes where
    show value = case value of
        Min n   -> "\"minimum\" : " ++ show n
        ExMin n -> "\"exclusiveMinimum\" : " ++ show n
        Max n   -> "\"maximum\" : " ++ show n
        ExMax n -> "\"exclusiveMaximum\" : " ++ show n
        Mult n  -> "\"multipleOf\" : " ++ show n

instance Show JSONObjectRes where
    show value = case value of
        Prop l        -> "\"properties\" : {\n  " ++ intercalate ",\n  " (map showProp l) ++ "\n}"
        AddProp True  -> "\"additionalProperties\" : true"
        AddProp False -> "\"additionalProperties\" : false"
        Required l    -> "\"required\" : [" ++ intercalate ", " (map show l) ++ "]"
        MinProp n     -> "\"minProperties\" : " ++ show n
        MaxProp n     -> "\"maxProperties\" : " ++ show n

instance Show JSONArrayRes where 
    show value = case value of
        Items s           -> "\"items\" : { " ++ show s ++ " }"
        PrefixItems l     -> "\"prefixItems\" : [\n{ " ++ intercalate " }, { " (map show l) ++ " }\n]"
        MinItems n        -> "\"minItems\" : " ++ show n
        MaxItems n        -> "\"maxItems\" : " ++ show n
        UniqueItems True  -> "\"uniqueItems\" : true"
        UniqueItems False -> "\"uniqueItems\" : false"
