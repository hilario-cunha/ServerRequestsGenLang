module TemplateSimpleGet
    ( TemplateSimpleGet(..)
    , createAndWriteToFileTemplateSimpleGet
    , MethodInfo(..)
    , MethodTryTo(..)
    , UrlBuilder(..)
    , UrlField(..)
    , UrlPart(..)
    , UrlQueryPart()
    , UrlQueryPartValue()
    , MyField(..)
    , mkFormalParamMyField
    , ResponseT(..)
    , mkUrlQueryPartVar
    , mkUrlQueryPartLiteral
    , extractNameFromMyField
    , extractFieldsFromMethodInfo
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import CSharpGen

data MyField= IntField String
            | StringField String
            | StringNotEmptyField String
            | StringNotEmptyArrayField String
            | DateTimeField String
            | DateTimeNullableField String
            | CustomField String String
            deriving Show

extractNameFromMyField :: MyField -> String
extractNameFromMyField (IntField n) = n
extractNameFromMyField (StringField n) = n
extractNameFromMyField (StringNotEmptyField n) = n
extractNameFromMyField (StringNotEmptyArrayField n) = n
extractNameFromMyField (DateTimeField n) = n
extractNameFromMyField (DateTimeNullableField n) = n
extractNameFromMyField (CustomField _ n) = n

mkFormalParamMyField (IntField n) = mkFormalParam "int" n
mkFormalParamMyField (StringField n) = mkFormalParam "string" n
mkFormalParamMyField (StringNotEmptyField n) = mkFormalParam "StringNotEmpty" n
mkFormalParamMyField (StringNotEmptyArrayField n) = mkFormalParam "StringNotEmpty[]" n
mkFormalParamMyField (DateTimeField n) = mkFormalParam "DateTime" n
mkFormalParamMyField (DateTimeNullableField n) = mkFormalParam "DateTime?" n
mkFormalParamMyField (CustomField t n) = mkFormalParam t n


mkArgs fields = map mkFormalParamMyField fields

data TemplateSimpleGet = TemplateSimpleGet [String] String [MethodTryTo]
    deriving Show

data UrlField = UrlField String MyField String
    deriving Show
data UrlPart    = UrlPartLit String
                | UrlPartVar UrlField
    deriving Show

data UrlQueryPartValue = UrlQueryPartLit String | UrlQueryPartVar UrlField
    deriving Show
data UrlQueryPart = UrlQueryPart String UrlQueryPartValue
    deriving Show

mkUrlQueryPartVar :: String -> String -> MyField -> String -> UrlQueryPart
mkUrlQueryPartVar n prefix field suffix = UrlQueryPart n $ UrlQueryPartVar $ UrlField prefix field suffix

mkUrlQueryPartLiteral :: String -> String -> UrlQueryPart
mkUrlQueryPartLiteral n literal = UrlQueryPart n $ UrlQueryPartLit literal

data UrlBuilder = UrlBuilder [UrlPart] [UrlQueryPart]
    deriving Show


data ResponseT  = ResponseT String
                | ResponseTArray String
                deriving Show

type MethodName = String

data MethodInfo = MethodInfo MethodName ResponseT [MyField]
    deriving Show

extractFieldsFromMethodInfo :: MethodInfo -> [MyField]
extractFieldsFromMethodInfo (MethodInfo _ _ fields) = fields

data MethodTryTo= MethodTryToGet MethodInfo UrlBuilder
                | MethodTryToPost MethodInfo String UrlBuilder
                deriving Show

createAndWriteToFileTemplateSimpleGet templateData  = 
    let 
        createServerRequestsFile = createTemplateSimpleGet templateData
        fileName = "output/" ++ (className (classDefinition createServerRequestsFile)) ++ ".GenTemplateSimpleGet.cs"
        cu = mkCu createServerRequestsFile
    in createAndWriteToFile fileName cu

mkCu = mkNamespaceWithClass mkTemplateSimpleGetClass
createAndWriteToFile fileName cu  = writeFile fileName $ prettyPrint cu      
    
createTemplateSimpleGet (TemplateSimpleGet extraUsings functionalityName methodsTryTo) = 
    createNamespaceWithClass
        usings 
        namespace
        (createClassWithMethods 
            cn
            (mkTemplateSimpleGetCtor cn)
            (map mkTemplateSimpleMethod methodsTryTo)
        )
    where 
        usings = ("Tlantic.Server.Core" : "System" : extraUsings)
        namespace = ("Tlantic.Server." ++ functionalityName)
        cn = functionalityName ++ "ServerRequests"
        mkTemplateSimpleMethod (MethodTryToGet mi u) = mkTemplateSimpleGetMethod mi u
        mkTemplateSimpleMethod (MethodTryToPost mi dataT u) = mkTemplateSimplePostMethod mi dataT u

mkTemplateSimpleGetMethodBody urlGet innerResponseTA = (mkUrlBuilder urlGet ++ [returnStatement])
    where
        returnStatement = mkReturn $ Invocation tryToGetMemberAccess trytoGetArgs
        trytoGetArgs = [mkSimpleNameArgument "urlBuilder"]
        tryToGetMemberAccess = MemberAccess $ mkPrimaryMemberAccessWithTypeArguments (mkSimpleName "serverConfig") "TryToGet" [innerResponseTA]

mkTemplateSimpleGetMethod (MethodInfo methodName responseT args) urlGet = 
    mkMethodMemberDeclaration [Public] returnType methodName (mkArgs args) (mkTemplateSimpleGetMethodBody urlGet innerResponseTA)
    where 
        returnType = mkTypeNamedWithTypeArguments "IChoiceGetRequestWithRetry" [responseTA, networkErrorTA]
        responseTA = TypeArgument (mkTypeNamedWithTypeArguments "Response" [innerResponseTA])
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")
        innerResponseTA = mkInnerResponseTA responseT

mkTemplateSimplePostMethod (MethodInfo methodName responseT args) dataT urlGet = mkMethodMemberDeclaration [Public] methodReturnT methodName methodArgs body
    where 
        methodReturnT = (mkTypeNamedWithTypeArguments "IChoicePostRequestWithRetry" [responseTA, networkErrorTA])
        responseTA = mkInnerResponseTA responseT
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")

        methodArgs = mkArgs (args ++ [CustomField dataT "data"])
        body  = mkTemplateSimplePostMethodBody methodName responseTA dataT args urlGet
        mkTemplateSimplePostMethodBody methodName responseTTypeArgument dataT args urlGet = (mkUrlBuilder urlGet ++ [ret])
            where 
                ret = mkReturnServerConfig "TryToPost" [(mkTypeNamedTypeArgument dataT), responseTTypeArgument] [mkSimpleNameArgument "urlBuilder", mkSimpleNameArgument "data"]
                mkReturnServerConfig mn tArgs args = Return (Just (Invocation (MemberAccess $ PrimaryMemberAccess (SimpleName (Identifier "serverConfig") [] ) (Identifier mn) tArgs) args))

mkTemplateSimpleGetClass classWithMethods = 
    mkPublicClass cn cb
    where 
        cn = className classWithMethods
        cb = mkTemplateSimpleGetClassBody cn classWithMethods

mkTemplateSimpleGetClassBody ctorName classWithMethods = (mkServerConfigField : ctor : ms)
    where 
        mkServerConfigField = mkField "ServerConfig" "serverConfig"
        ctor = mkTemplateSimpleGetCtor ctorName
        ms = methods classWithMethods

mkTemplateSimpleGetCtor ctorName = mkConstructorMemberDeclaration [Internal] ctorName [serverConfigFormalParam] [serverConfigAssign]
    where
        serverConfigFormalParam = mkFormalParam "ServerConfig" "serverConfig"
        serverConfigAssign = mkAssignStatement "this.serverConfig" "serverConfig"

        
mkInnerResponseTA (ResponseT t) = mkTypeNamedTypeArgument t
mkInnerResponseTA (ResponseTArray t) = mkTypeArrayTypeArgument t

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
        urlPartAsArgument (UrlPartVar (UrlField prefix (IntField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".ToString()")
        urlPartAsArgument (UrlPartVar (UrlField prefix (StringField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix n
        urlPartAsArgument (UrlPartVar (UrlField prefix (StringNotEmptyField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".Value")
        urlPartAsArgument (UrlPartVar (UrlField prefix (StringNotEmptyArrayField n) suffix)) = error "StringNotEmptyArrayField Not supported in url part"
        urlPartAsArgument (UrlPartVar (UrlField prefix (DateTimeField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
        urlPartAsArgument (UrlPartVar (UrlField prefix (DateTimeNullableField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix n
        urlPartAsArgument (UrlPartVar (UrlField prefix (CustomField t n) suffix)) = error "CustomField Not supported in url part"

        addPrefixAndSuffixAsArgument prefix suffix n = mkSimpleNameArgument $ addPrefix prefix ++ n ++ addSuffix suffix
        addPrefix prefix = if(not $ null prefix) then "\"" ++ prefix ++ "\" + " else []
        addSuffix suffix = if(not $ null suffix) then " + \"" ++ suffix ++ "\"" else []
        
        urlQueryPartValueAsArgument (UrlQueryPartLit v) = mkLiteralStringArgument v
        urlQueryPartValueAsArgument (UrlQueryPartVar (UrlField prefix (IntField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".ToString()")
        urlQueryPartValueAsArgument (UrlQueryPartVar (UrlField prefix (StringField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix n
        urlQueryPartValueAsArgument (UrlQueryPartVar (UrlField prefix (StringNotEmptyField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix (n ++ ".Value")
        urlQueryPartValueAsArgument (UrlQueryPartVar (UrlField prefix (StringNotEmptyArrayField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix n
        urlQueryPartValueAsArgument (UrlQueryPartVar (UrlField prefix (DateTimeField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
        urlQueryPartValueAsArgument (UrlQueryPartVar (UrlField prefix (DateTimeNullableField n) suffix)) = addPrefixAndSuffixAsArgument prefix suffix ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ".Value)")
        urlQueryPartValueAsArgument (UrlQueryPartVar (UrlField prefix (CustomField _ n) suffix)) = error "CustomField Not supported in url query part"

        urlQueryPartAsArgument (UrlQueryPart n v) = mkNewArgument "UrlQueryParameter" $ [mkLiteralStringArgument n, urlQueryPartValueAsArgument v]
