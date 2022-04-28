module JSONSchemaGenerator where

import Test.QuickCheck
import JSONSchemaSyntax
import Control.Monad

validNames :: [String]
validNames = [ c : s | s <- "": validNames, c <- ['a'..'z'] ++ ['0'..'9'] ]

genNullSchema :: Gen JSONSchema
genNullSchema = pure JSONNullSchema

genBoolSchema :: Gen JSONSchema
genBoolSchema = pure JSONBoolSchema

genStringSchema :: Gen JSONSchema
genStringSchema = JSONStringSchema <$> genStringRes 

genStringRes :: Gen [JSONStringRes]
genStringRes = do n1 <- abs <$> arbitrary
                  n2 <- abs <$> arbitrary
                  sublistOf [MinLength (min n1 n2), MaxLength (max n1 n2)]

genIntegerSchema :: Gen JSONSchema
genIntegerSchema = JSONIntegerSchema <$> genNumberRes

genNumberSchema :: Gen JSONSchema
genNumberSchema = JSONNumberSchema <$> genNumberRes

genNumberRes :: Gen [JSONNumberRes]
genNumberRes = do n1 <- arbitrary
                  n2 <- arbitrary
                  n3 <- abs <$> arbitrary
                  e1 <- elements [Min (min n1 n2), ExMin (min n1 n2)]
                  e2 <- elements [Max (max n1 n2), ExMax (max n1 n2)]
                  sublistOf [e1, e2, Mult n3]

genObjectSchema :: Int -> Gen JSONSchema
genObjectSchema n = JSONObjectSchema <$> genObjectRes n

genObjectRes :: Int -> Gen [JSONObjectRes]
genObjectRes n = do n1 <- abs <$> arbitrary
                    n2 <- abs <$> arbitrary
                    n3 <- abs <$> arbitrary
                    np <- choose (0, n3)
                    vn <- sublistOf (take np validNames)
                    s  <- (mapM (\v -> genSchema (n `div` 2)) vn)
                    a  <- AddProp <$> arbitrary
                    r  <- Required <$> sublistOf vn
                    sublistOf [Prop (zip vn s), a, r, MinProp (min n1 n2), MaxProp (max n1 n2)]

genArraySchema :: Int -> Gen JSONSchema
genArraySchema n = JSONArraySchema <$> genArrayRes n

genArrayRes :: Int -> Gen [JSONArrayRes]
genArrayRes n = do n1 <- abs <$> arbitrary
                   n2 <- abs <$> arbitrary
                   i1 <- Items <$> genSchema (n `div` 2)
                   i2 <- PrefixItems <$> listOf (genSchema (n `div` 2))
                   i  <- elements [i1, i2]
                   u  <- UniqueItems <$> arbitrary
                   sublistOf [i, u, MinItems (min n1 n2), MaxItems (max n1 n2)]

genSchema :: Int -> Gen JSONSchema
genSchema n = if n < 5 
    then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
    else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
    where 
        scalarGens      = [genNullSchema, genBoolSchema, genStringSchema, genNumberSchema]
        compositeGens n = [genObjectSchema n, genArraySchema n]

instance Arbitrary JSONSchema where
    arbitrary   = sized genSchema
--    shrink      = genericShrink