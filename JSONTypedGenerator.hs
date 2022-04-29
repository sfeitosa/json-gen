module JSONTypedGenerator where

import Test.QuickCheck
import JSONSyntax
import JSONSchemaSyntax
import Data.Maybe

-- Restrictions

data StringRes = StringRes { 
                   minlen :: Maybe Integer, 
                   maxlen :: Maybe Integer
                }
                deriving Show

newStringRes :: StringRes
newStringRes = StringRes Nothing Nothing

toStringRes :: StringRes -> [JSONStringRes] -> StringRes
toStringRes sr [] = sr
toStringRes sr (x:xs) = 
    case x of
      MinLength n -> toStringRes (sr { minlen = (Just n) }) xs
      MaxLength n -> toStringRes (sr { maxlen = (Just n) }) xs

data NumberRes = NumberRes {
                    minn :: Maybe Integer,
                    exminn :: Maybe Integer,
                    maxn :: Maybe Integer,
                    exmaxn :: Maybe Integer,
                    multn :: Maybe Integer
                }
                deriving Show

newNumberRes :: NumberRes 
newNumberRes = NumberRes Nothing Nothing Nothing Nothing Nothing

toNumberRes :: NumberRes -> [JSONNumberRes] -> NumberRes
toNumberRes nr [] = nr
toNumberRes nr (x:xs) = 
    case x of
        Min n   -> toNumberRes (nr { minn = (Just n) }) xs
        ExMin n -> toNumberRes (nr { exminn = (Just n) }) xs
        Max n   -> toNumberRes (nr { maxn = (Just n) }) xs
        ExMax n -> toNumberRes (nr { exmaxn = (Just n) }) xs
        Mult n  -> toNumberRes (nr { multn = (Just n) }) xs

data ObjectRes = ObjectRes {
                    prop :: Maybe [(String, JSONSchema)],
                    addprop :: Maybe Bool,
                    req :: Maybe [String],
                    minp :: Maybe Integer,
                    maxp :: Maybe Integer
                }
                deriving Show

newObjectRes :: ObjectRes
newObjectRes = ObjectRes Nothing Nothing Nothing Nothing Nothing

toObjectRes :: ObjectRes -> [JSONObjectRes] -> ObjectRes
toObjectRes or [] = or
toObjectRes or (x:xs) = 
    case x of 
        Prop l     -> toObjectRes (or { prop = (Just l) }) xs
        AddProp b  -> toObjectRes (or { addprop = (Just b) }) xs
        Required l -> toObjectRes (or { req = (Just l) }) xs
        MinProp n  -> toObjectRes (or { minp = (Just n) }) xs
        MaxProp n  -> toObjectRes (or { maxp = (Just n) }) xs

data ArrayRes = ArrayRes {
                   items :: Maybe JSONSchema,
                   prefixi :: Maybe [JSONSchema],
                   mini :: Maybe Integer,
                   maxi :: Maybe Integer,
                   uniq :: Maybe Bool
               }

newArrayRes :: ArrayRes
newArrayRes = ArrayRes Nothing Nothing Nothing Nothing Nothing

toArrayRes :: ArrayRes -> [JSONArrayRes] -> ArrayRes
toArrayRes ar [] = ar
toArrayRes ar (x:xs) = 
    case x of 
        Items i       -> toArrayRes (ar { items = (Just i) }) xs
        PrefixItems l -> toArrayRes (ar { prefixi = (Just l) }) xs
        MinItems n    -> toArrayRes (ar { mini = (Just n) }) xs
        MaxItems n    -> toArrayRes (ar { maxi = (Just n) }) xs
        UniqueItems b -> toArrayRes (ar { uniq = (Just b) }) xs

-- Auxiliary functions

validNames :: [String]
validNames = [ c : s | s <- "": validNames, c <- ['a'..'z'] ]

minM :: Maybe Integer -> Maybe Integer -> Maybe Integer
minM (Just n1) (Just n2) = Just (min n1 n2)
minM n Nothing = n
minM Nothing n = n
minN _ _ = Nothing

maxM :: Maybe Integer -> Maybe Integer -> Maybe Integer
maxM (Just n1) (Just n2) = Just (max n1 n2)
maxM n Nothing = n
maxM Nothing n = n
maxN _ _ = Nothing

-- Generators

genNull :: Gen JSONValue
genNull = pure JSONNull

genBool :: Gen JSONValue
genBool = JSONBool <$> arbitrary

genTypedString :: StringRes -> Gen JSONValue
genTypedString (StringRes (Just min) (Just max)) = 
    JSONString <$> listOf (elements ['a'..'z']) 
      `suchThat` (\s -> length s >= fromIntegral min && length s <= fromIntegral max)
genTypedString (StringRes (Just min) _) = 
    JSONString <$> listOf (elements ['a'..'z']) 
      `suchThat` (\s -> length s >= fromIntegral min)
genTypedString (StringRes _ (Just max)) = 
    JSONString <$> listOf (elements ['a'..'z']) 
      `suchThat` (\s -> length s <= fromIntegral max)
genTypedString _ = 
    JSONString <$> listOf (elements ['a'..'z'])

genTypedInteger :: NumberRes -> Gen JSONValue
genTypedInteger nr = -- @todo: implement multipleOf
    case (maxM (minn nr) (exminn nr), minM (maxn nr) (exmaxn nr)) of -- @todo: fix the exminn e exmaxn index
        (Just n1, Just n2) ->
            JSONInteger <$> arbitrary `suchThat` (\n -> n >= n1 && n <= n2)
        (Just n, Nothing)  -> 
            JSONInteger <$> arbitrary `suchThat` (\n' -> n' >= n)
        (Nothing, Just n)  -> 
            JSONInteger <$> arbitrary `suchThat` (\n' -> n' <= n)
        _            -> JSONInteger <$> arbitrary 

genTypedNumber :: NumberRes -> Gen JSONValue
genTypedNumber nr = -- @todo: implement multipleOf
    case (maxM (minn nr) (exminn nr), minM (maxn nr) (exmaxn nr)) of -- @todo: fix the exminn e exmaxn index
        (Just n1, Just n2) ->
            JSONNumber <$> arbitrary `suchThat` (\n -> n >= n1 && n < n2) <*> listOf (choose (0, 9)) <*> pure 0
        (Just n, Nothing)  -> 
            JSONNumber <$> arbitrary `suchThat` (\n' -> n' >= n) <*> listOf (choose (0, 9)) <*> pure 0
        (Nothing, Just n)  -> 
            JSONNumber <$> arbitrary `suchThat` (\n' -> n' < n) <*> listOf (choose (0, 9)) <*> pure 0
        _            -> JSONNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary


genTypedObject :: Int -> ObjectRes -> Gen JSONValue
genTypedObject n or = 
    case or of 
        _ -> fmap JSONObject (listOf (objKV (n `div` 2)))
    where objKV n = (,) <$> elements (take 1000 validNames) <*> genUntypedValue n

genTypedValue :: Int -> JSONSchema -> Gen JSONValue
genTypedValue n s = 
    case s of 
        JSONNullSchema      -> genNull
        JSONBoolSchema      -> genBool
        JSONStringSchema l  -> genTypedString  $ toStringRes newStringRes l
        JSONNumberSchema l  -> genTypedNumber  $ toNumberRes newNumberRes l
        JSONIntegerSchema l -> genTypedInteger $ toNumberRes newNumberRes l
        JSONObjectSchema l  -> genTypedObject (n `div` 2) $ toObjectRes newObjectRes l

genUntypedValue :: Int -> Gen JSONValue
genUntypedValue n = if n < 5 
    then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
    else frequency [(2, oneof scalarGens), (1, oneof (compositeGens n))]
    where 
        scalarGens      = [genNull, genBool, genTypedNumber newNumberRes, genTypedInteger newNumberRes, genTypedString newStringRes]
        compositeGens n = [genTypedObject n newObjectRes]
    
