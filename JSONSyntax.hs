{-# LANGUAGE DeriveGeneric #-}

module JSONSyntax where

import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt)
import Data.List (intercalate)
import Numeric (showHex)
import GHC.Generics (Generic)

-- Definition of JSON values

data JSONValue = JSONNull
               | JSONBool Bool
               | JSONString String
               | JSONInteger Integer
               | JSONNumber { int :: Integer, frac :: [Int], exponent :: Integer }
               | JSONArray [JSONValue]
               | JSONObject [(String, JSONValue)]
               deriving (Eq, Generic)

-- Pretty printing JSON values

instance Show JSONValue where
    show value = case value of
        JSONNull            -> "null"
        JSONBool True       -> "true"
        JSONBool False      -> "false"
        JSONString s        -> showJSONString s
        JSONInteger n       -> show n
        JSONNumber s [] 0   -> show s 
        JSONNumber s f 0    -> show s ++ "." ++ concatMap show f
        JSONNumber s [] e   -> show s ++ "e" ++ show e 
        JSONNumber s f e    -> show s ++ "." ++ concatMap show f ++ "e" ++ show e 
        JSONArray a         -> "[" ++ intercalate ", " (map show a) ++ "]" 
        JSONObject o           -> "{" ++ intercalate ", " (map showKV o) ++ "}"
        where 
            showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""
            showKV (k, v) = showJSONString k ++ ": " ++ show v

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
    '\'' -> "'"
    '\"' -> "\\\""
    '\\' -> "\\\\"
    '/'  -> "\\/"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c 
    _    -> [c]
    where 
        showJSONNonASCIIChar c = 
            let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a 

