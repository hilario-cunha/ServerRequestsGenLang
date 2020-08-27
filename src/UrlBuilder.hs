module UrlBuilder
    ( UrlBuilder(..)
    , UrlPart(..)
    , UrlPartField(..)
    , UrlQueryPart(..)
    , UrlQueryPartValue(..)
    , UrlQueryPartField(..)
    , mkUrlBuilder
    , mkMethod
    , extractNamesFromUrlBuilder
    , mkUlrBuilderMethod
    , UlrBuilderMethod(..)
    , invokeUrlBuilderMethod
    ) where

import Language.CSharp.Syntax
import CSharpGen
import Data.Maybe (catMaybes)

data UrlBuilder = UrlBuilder [UrlPart] [UrlQueryPart]
    deriving Show

data UrlPart    = UrlPartLit String | UrlPartVar String UrlPartField String
    deriving Show

data UrlPartField= IntPartField String
    | StringPartField String
    | StringNotEmptyPartField String
    | DateTimePartField String
    | DateTimeNullablePartField String
    deriving Show

extractNameFromUrlPart :: UrlPart -> Maybe String
extractNameFromUrlPart (UrlPartLit _) = Nothing
extractNameFromUrlPart (UrlPartVar _ f _) = Just $ extractNameFromUrlPartField f

extractNameFromUrlPartField :: UrlPartField -> String
extractNameFromUrlPartField (IntPartField n) = n
extractNameFromUrlPartField (StringPartField n) = n
extractNameFromUrlPartField (StringNotEmptyPartField n) = n
extractNameFromUrlPartField (DateTimePartField n) = n
extractNameFromUrlPartField (DateTimeNullablePartField n) = n

data UrlQueryPart = UrlQueryPart String UrlQueryPartValue
    deriving Show

data UrlQueryPartValue = UrlQueryPartLit String | UrlQueryPartVar String UrlQueryPartField String
    deriving Show

data UrlQueryPartField  = IntQueryPartField String
    | StringQueryPartField String
    | StringNotEmptyQueryPartField String
    | StringNotEmptyArrayQueryPartField String
    | DateTimeQueryPartField String
    | DateTimeNullableQueryPartField String
    deriving Show

extractNameFromUrlQueryPart :: UrlQueryPart -> Maybe String
extractNameFromUrlQueryPart (UrlQueryPart _ v) = extractNameFromUrlQueryPartValue v

extractNameFromUrlQueryPartValue :: UrlQueryPartValue -> Maybe String
extractNameFromUrlQueryPartValue (UrlQueryPartLit _) = Nothing
extractNameFromUrlQueryPartValue (UrlQueryPartVar _ f _) = Just $ extractNameFromUrlQueryPartField f

extractNameFromUrlQueryPartField :: UrlQueryPartField -> String
extractNameFromUrlQueryPartField (IntQueryPartField n) = n
extractNameFromUrlQueryPartField (StringQueryPartField n) = n
extractNameFromUrlQueryPartField (StringNotEmptyQueryPartField n) = n
extractNameFromUrlQueryPartField (StringNotEmptyArrayQueryPartField n) = n
extractNameFromUrlQueryPartField (DateTimeQueryPartField n) = n
extractNameFromUrlQueryPartField (DateTimeNullableQueryPartField n) = n

extractNamesFromUrlParts :: [UrlPart] -> [String]
extractNamesFromUrlParts parts = catMaybes $ map extractNameFromUrlPart parts

extractNamesFromUrlQueryParts :: [UrlQueryPart] -> [String]
extractNamesFromUrlQueryParts queryParts = catMaybes $ map extractNameFromUrlQueryPart queryParts


extractNamesFromUrlBuilder :: UrlBuilder -> [String]
extractNamesFromUrlBuilder (UrlBuilder parts queryParts) = extractNamesFromUrlParts parts ++ extractNamesFromUrlQueryParts queryParts

mkUrlBuilder :: UrlBuilder -> [Statement]
mkUrlBuilder (UrlBuilder parts queryParts) = 
    [
        var "parts" "UrlParts" (map urlPartAsArgument parts), 
        var "queryParts" "UrlQueryParameters" (map urlQueryPartAsArgument queryParts),
        var "urlBuilder" "UrlBuilder" [mkSimpleNameArgument "parts", mkSimpleNameArgument "queryParts"]
    ]
    where 
        var n t args = mkAndInitLocalVar n $ mkNew t args 

        urlPartAsArgument (UrlPartLit l) = mkLiteralStringArgument l
        urlPartAsArgument (UrlPartVar prefix (IntPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".ToString()")
        urlPartAsArgument (UrlPartVar prefix (StringPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n
        urlPartAsArgument (UrlPartVar prefix (StringNotEmptyPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".Value")
        urlPartAsArgument (UrlPartVar prefix (DateTimePartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
        urlPartAsArgument (UrlPartVar prefix (DateTimeNullablePartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n

        addPrefixAndSuffixAsArgument prefix suffix n = mkSimpleNameArgument $ addPrefix prefix ++ n ++ addSuffix suffix
        addPrefix prefix = if(not $ null prefix) then "\"" ++ prefix ++ "\" + " else []
        addSuffix suffix = if(not $ null suffix) then " + \"" ++ suffix ++ "\"" else []
        
        urlQueryPartValueAsArgument (UrlQueryPartLit v) = mkLiteralStringArgument v
        urlQueryPartValueAsArgument (UrlQueryPartVar prefix (IntQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".ToString()")
        urlQueryPartValueAsArgument (UrlQueryPartVar prefix (StringQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n
        urlQueryPartValueAsArgument (UrlQueryPartVar prefix (StringNotEmptyQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".Value")
        urlQueryPartValueAsArgument (UrlQueryPartVar prefix (StringNotEmptyArrayQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n
        urlQueryPartValueAsArgument (UrlQueryPartVar prefix (DateTimeQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
        urlQueryPartValueAsArgument (UrlQueryPartVar prefix (DateTimeNullableQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ".Value)")

        urlQueryPartAsArgument (UrlQueryPart n v) = mkNewArgument "UrlQueryParameter" $ [mkLiteralStringArgument n, urlQueryPartValueAsArgument v]

data UlrBuilderMethod = UlrBuilderMethod String UrlBuilder 
    deriving Show

mkUlrBuilderMethod :: UlrBuilderMethod -> MemberDeclaration
mkUlrBuilderMethod (UlrBuilderMethod methodName urlBuilder) = mkMethod methodName urlBuilder

mkMethod :: String -> UrlBuilder -> MemberDeclaration
mkMethod methodName (UrlBuilder parts queryParts) = mkMethodMemberDeclaration [Public] (mkTypeNamed "UrlBuilder") methodName params body
    where
        params = mkFormalParamsFromUrlParts ++ mkFormalParamsFromUrlQueryParts
        mkFormalParamsFromUrlParts = catMaybes $ map mkFormalParamsFromUrlPart parts

        mkFormalParamsFromUrlPart (UrlPartVar _ f _) = Just $ mkFormalParamFromUrlPartField f
        mkFormalParamsFromUrlPart (UrlPartLit _) = Nothing

        mkFormalParamFromUrlPartField (IntPartField n) = mkFormalParam "int" n
        mkFormalParamFromUrlPartField (StringPartField n) = mkFormalParam "string" n
        mkFormalParamFromUrlPartField (StringNotEmptyPartField n) = mkFormalParam "StringNotEmpty" n
        mkFormalParamFromUrlPartField (DateTimePartField n) = mkFormalParam "DateTime" n
        mkFormalParamFromUrlPartField (DateTimeNullablePartField n) = mkFormalParam "DateTime?" n

        mkFormalParamsFromUrlQueryParts = catMaybes $ map mkFormalParamsFromUrlQueryPart queryParts

        mkFormalParamsFromUrlQueryPart (UrlQueryPart _ v) = mkFormalParamsFromUrlQueryPartValue v

        mkFormalParamsFromUrlQueryPartValue (UrlQueryPartVar _ f _) = Just $ mkFormalParamFromUrlQueryPartField f
        mkFormalParamsFromUrlQueryPartValue (UrlQueryPartLit _) = Nothing

        mkFormalParamFromUrlQueryPartField (IntQueryPartField n) = mkFormalParam "int" n
        mkFormalParamFromUrlQueryPartField (StringQueryPartField n) = mkFormalParam "string" n
        mkFormalParamFromUrlQueryPartField (StringNotEmptyQueryPartField n) = mkFormalParam "StringNotEmpty" n
        mkFormalParamFromUrlQueryPartField (StringNotEmptyArrayQueryPartField n) = mkFormalParam "StringNotEmpty[]" n
        mkFormalParamFromUrlQueryPartField (DateTimeQueryPartField n) = mkFormalParam "DateTime" n
        mkFormalParamFromUrlQueryPartField (DateTimeNullableQueryPartField n) = mkFormalParam "DateTime?" n

        body = 
            [
                var "parts" "UrlParts" (map urlPartAsArgument parts), 
                var "queryParts" "UrlQueryParameters" (map urlQueryPartAsArgument queryParts),
                mkReturn $ mkNew "UrlBuilder" [mkSimpleNameArgument "parts", mkSimpleNameArgument "queryParts"]
            ]
            where 
                var n t args = mkAndInitLocalVar n $ mkNew t args 

                urlPartAsArgument (UrlPartLit l) = mkLiteralStringArgument l
                urlPartAsArgument (UrlPartVar prefix (IntPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".ToString()")
                urlPartAsArgument (UrlPartVar prefix (StringPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n
                urlPartAsArgument (UrlPartVar prefix (StringNotEmptyPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".Value")
                urlPartAsArgument (UrlPartVar prefix (DateTimePartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
                urlPartAsArgument (UrlPartVar prefix (DateTimeNullablePartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n

                addPrefixAndSuffixAsArgument prefix suffix n = mkSimpleNameArgument $ addPrefix prefix ++ n ++ addSuffix suffix
                addPrefix prefix = if(not $ null prefix) then "\"" ++ prefix ++ "\" + " else []
                addSuffix suffix = if(not $ null suffix) then " + \"" ++ suffix ++ "\"" else []
                
                urlQueryPartValueAsArgument (UrlQueryPartLit v) = mkLiteralStringArgument v
                urlQueryPartValueAsArgument (UrlQueryPartVar prefix (IntQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".ToString()")
                urlQueryPartValueAsArgument (UrlQueryPartVar prefix (StringQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n
                urlQueryPartValueAsArgument (UrlQueryPartVar prefix (StringNotEmptyQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".Value")
                urlQueryPartValueAsArgument (UrlQueryPartVar prefix (StringNotEmptyArrayQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix n
                urlQueryPartValueAsArgument (UrlQueryPartVar prefix (DateTimeQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
                urlQueryPartValueAsArgument (UrlQueryPartVar prefix (DateTimeNullableQueryPartField n) suffix) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ".Value)")

                urlQueryPartAsArgument (UrlQueryPart n v) = mkNewArgument "UrlQueryParameter" $ [mkLiteralStringArgument n, urlQueryPartValueAsArgument v]

invokeUrlBuilderMethod :: UlrBuilderMethod -> Expression
invokeUrlBuilderMethod (UlrBuilderMethod methodName urlBuilder) = mkInvokeMethod methodName $ urlBuilderArgs
    where 
        urlBuilderArgs = map mkSimpleNameArgument $ extractNamesFromUrlBuilder urlBuilder