module UrlBuilder
    ( UrlBuilder(..)
    , UrlPart(..)
    , UrlPartField(..)
    , UrlQueryPart(..)
    , UrlQueryPartValue(..)
    , UrlQueryPartField(..)
    , mkUrlBuilder
    ) where

import Language.CSharp.Syntax
import CSharpGen

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