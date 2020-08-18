module TemplateSimpleGet
    ( TemplateSimpleGet(..)
    , createAndWriteToFileTemplateSimpleGet
    , MethodInfo(..)
    , MethodTryTo(..)
    , UrlGet(..)
    , UrlPart(..)
    , UrlQueryPart(..)
    , MyField(..)
    , mkFormalParamMyField
    , mkUrlQueryPart
    , ResponseT(..)
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen

data MyField = IntField String
    | StringField String
    | StringNotEmptyField String
    | StringNotEmptyArrayField String
    | DateTimeField String
    | DateTimeNullableField String
    | CustomField String String

mkFormalParamMyField (IntField n) = mkFormalParam "int" n
mkFormalParamMyField (StringField n) = mkFormalParam "string" n
mkFormalParamMyField (StringNotEmptyField n) = mkFormalParam "StringNotEmpty" n
mkFormalParamMyField (StringNotEmptyArrayField n) = mkFormalParam "StringNotEmpty[]" n
mkFormalParamMyField (DateTimeField n) = mkFormalParam "DateTime" n
mkFormalParamMyField (DateTimeNullableField n) = mkFormalParam "DateTime?" n
mkFormalParamMyField (CustomField t n) = mkFormalParam t n

mkUrlQueryPart queryPartName (IntField n) = UrlQueryPart queryPartName (n ++ ".ToString()")
mkUrlQueryPart queryPartName (StringField n) = UrlQueryPart queryPartName n
mkUrlQueryPart queryPartName (StringNotEmptyField n) = UrlQueryPart queryPartName (n ++ ".Value")
mkUrlQueryPart queryPartName (StringNotEmptyArrayField n) = UrlQueryPart queryPartName n
mkUrlQueryPart queryPartName (DateTimeField n) = UrlQueryPart queryPartName ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
mkUrlQueryPart queryPartName (DateTimeNullableField n) = UrlQueryPart queryPartName ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ".Value)")
mkUrlQueryPart queryPartName (CustomField _ n) = error "CustomField Not supported in url query part"

mkArgs fields = map mkFormalParamMyField fields

data TemplateSimpleGet = TemplateSimpleGet [Using] String [MethodTryTo]

data UrlPart = UrlPartLit String
    | UrlPartVar MyField
data UrlQueryPart = UrlQueryPart String String
data UrlGet = UrlGet [UrlPart] [UrlQueryPart]

type MethodName = String
data ResponseT = ResponseT String
        | ResponseTArray String
data MethodInfo = MethodInfo MethodName ResponseT [MyField]
data MethodTryTo = MethodTryToGet MethodInfo UrlGet
    | MethodTryToPost MethodInfo String UrlGet

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
        ((mkUsing "Tlantic.Server.Core") : (mkUsing "System") : extraUsings)
        ("Tlantic.Server." ++ functionalityName)
        (createClassWithMethods 
            cn
            (mkTemplateSimpleGetCtor cn)
            (map mkTemplateSimpleMethod methodsTryTo)
        )
    where 
        cn = functionalityName ++ "ServerRequests"
        mkTemplateSimpleMethod (MethodTryToGet mi u) = mkTemplateSimpleGetMethod mi u
        mkTemplateSimpleMethod (MethodTryToPost mi dataT u) = mkTemplateSimplePostMethod mi dataT u


mkTemplateSimpleGetClass classWithMethods = 
    mkClass cn cb
    where 
        cn = className classWithMethods
        cb = mkTemplateSimpleGetClassBody cn classWithMethods

mkTemplateSimpleGetClassBody ctorName classWithMethods = 
    (mkServerConfigField : ctor : ms)
    where 
        ctor = mkTemplateSimpleGetCtor ctorName
        ms = methods classWithMethods

mkTemplateSimpleGetCtor ctorName = mkCtor ctorName [serverConfigFormalParam] [serverConfigAssign]
    where
        serverConfigFormalParam = mkFormalParam "ServerConfig" "serverConfig"
        serverConfigAssign = mkAssign "this.serverConfig" "serverConfig"

mkForamParamFromUrlPart (UrlPartLit l) = mkStringLitArgument l
mkForamParamFromUrlPart (UrlPartVar (IntField n)) = mkSimpleNameArgument  n
mkForamParamFromUrlPart (UrlPartVar (StringField n)) = mkSimpleNameArgument n
mkForamParamFromUrlPart (UrlPartVar (StringNotEmptyField n)) = mkSimpleNameArgument (n ++ ".Value")
mkForamParamFromUrlPart (UrlPartVar (StringNotEmptyArrayField n)) = error "StringNotEmptyArrayField Not supported in url part"
mkForamParamFromUrlPart (UrlPartVar (DateTimeField n)) = mkSimpleNameArgument ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
mkForamParamFromUrlPart (UrlPartVar (DateTimeNullableField n)) = mkSimpleNameArgument n
mkForamParamFromUrlPart (UrlPartVar (CustomField t n)) = error "CustomField Not supported in url part"
        
mkInnerResponseTA (ResponseT t) = mkTypeNamedTypeArgument t
mkInnerResponseTA (ResponseTArray t) = mkTypeArrayTypeArgument t

mkTemplateSimpleGetMethodInfo (MethodInfo methodName responseT args) = mkMethodPublic methodReturnT methodName methodArgs
    where 
        methodReturnT = Just (mkTypeNamedArgs "IChoiceGetRequestWithRetry" [responseTA, networkErrorTA])
        responseTA = TypeArgument (mkTypeNamedArgs "Response" [ mkInnerResponseTA responseT])
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")
        methodArgs = mkArgs args

mkUrlBuilder urlGet = [partsVar, queryPartsVar, urlBuilderVar]
    where
        parts (UrlGet parts _) =  mkNew "UrlParts" (map mkForamParamFromUrlPart parts)
        partsVar = mkAndInitLocalVar "parts" $ parts urlGet
        queryPart (UrlQueryPart n v) = mkNewArgument "UrlQueryParameter" [mkStringLitArgument n, mkSimpleNameArgument v]
        queryParts (UrlGet _ q) =  mkNew "UrlQueryParameters" (map queryPart q)
        queryPartsVar = mkAndInitLocalVar "queryParts" (queryParts urlGet)
        urlBuilderVar = mkAndInitLocalVar "urlBuilder" $ mkNew "UrlBuilder" [mkSimpleNameArgument "parts", mkSimpleNameArgument "queryParts"]

mkTemplateSimpleGetMethodBody responseTTypeArgument urlGet = MethodStatementBody (mkUrlBuilder urlGet ++ [ret])
    where 
        ret = mkReturnServerConfig "TryToGet" [responseTTypeArgument] [mkSimpleNameArgument "urlBuilder"]

mkTemplateSimpleGetMethod methodInfo urlGet = mkTemplateSimpleGetMethodInfo methodInfo (ms methodInfo)
    where 
        ms (MethodInfo _ responseT _)  = mkTemplateSimpleGetMethodBody (mkInnerResponseTA responseT) urlGet
    
mkData methodName args = mkAndInitLocalVar "data" $ mkInvocation (map mkArg args)
    where
        mkArg (IntField n) = mkSimpleNameArgument  n
        mkArg (StringField n) = mkSimpleNameArgument n
        mkArg (StringNotEmptyField n) = mkSimpleNameArgument n
        mkArg (StringNotEmptyArrayField n) = mkSimpleNameArgument n
        mkArg (DateTimeField n) = mkSimpleNameArgument n
        mkArg (DateTimeNullableField n) = mkSimpleNameArgument n
        mkArg (CustomField t n) = mkSimpleNameArgument n
        mkInvocation args = Invocation (SimpleName (Identifier (methodName ++ "MapData")) []) args   
        
mkTemplateSimplePostMethodBody methodName responseTTypeArgument dataT args urlGet = MethodStatementBody (mkUrlBuilder urlGet ++ [dataC, ret])
    where 
        dataC = mkData methodName args
        ret = mkReturnServerConfig "TryToPost" [(mkTypeNamedTypeArgument dataT), responseTTypeArgument] [mkSimpleNameArgument "urlBuilder", mkSimpleNameArgument "data"]

mkReturnServerConfig mn tArgs args = Return (Just (Invocation (MemberAccess $ PrimaryMemberAccess (SimpleName (Identifier "serverConfig") [] ) (Identifier mn) tArgs) args))

mkTemplateSimplePostMethodInfo (MethodInfo methodName responseT args) = mkMethodPublic methodReturnT methodName methodArgs
    where 
        methodReturnT = Just (mkTypeNamedArgs "IChoicePostRequestWithRetry" [responseTA, networkErrorTA])
        responseTA = mkInnerResponseTA responseT
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")
        methodArgs = mkArgs args

mkTemplateSimplePostMethod methodInfo dataT urlGet = mkTemplateSimplePostMethodInfo methodInfo (ms methodInfo)
    where 
        ms (MethodInfo methodName responseT args)  = mkTemplateSimplePostMethodBody methodName (mkInnerResponseTA responseT) dataT args urlGet

mkServerConfigField = mkField "ServerConfig" "serverConfig"


