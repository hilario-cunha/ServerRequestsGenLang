module TemplateSimpleGet
    ( TemplateSimpleGet(..)
    , createAndWriteToFileTemplateSimpleGet
    , MethodInfo(..)
    , MethodTryTo(..)
    , UrlBuilder(..)
    , UrlPart(..)
    , UrlQueryPart()
    , UrlQueryPartValue()
    , MyField(..)
    , mkFormalParamMyField
    , ResponseT(..)
    , mkUrlQueryPartVar
    , mkUrlQueryPartLiteral
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

mkFormalParamMyField (IntField n) = mkFormalParam "int" n
mkFormalParamMyField (StringField n) = mkFormalParam "string" n
mkFormalParamMyField (StringNotEmptyField n) = mkFormalParam "StringNotEmpty" n
mkFormalParamMyField (StringNotEmptyArrayField n) = mkFormalParam "StringNotEmpty[]" n
mkFormalParamMyField (DateTimeField n) = mkFormalParam "DateTime" n
mkFormalParamMyField (DateTimeNullableField n) = mkFormalParam "DateTime?" n
mkFormalParamMyField (CustomField t n) = mkFormalParam t n


mkArgs fields = map mkFormalParamMyField fields

data TemplateSimpleGet = TemplateSimpleGet [String] String [MethodTryTo]

data UrlPart    = UrlPartLit String
                | UrlPartVar MyField

data UrlQueryPartValue = UrlQueryPartLit String | UrlQueryPartVar MyField
data UrlQueryPart = UrlQueryPart String UrlQueryPartValue

mkUrlQueryPartVar :: String -> MyField -> UrlQueryPart
mkUrlQueryPartVar n field = UrlQueryPart n $ UrlQueryPartVar field

mkUrlQueryPartLiteral :: String -> String -> UrlQueryPart
mkUrlQueryPartLiteral n literal = UrlQueryPart n $ UrlQueryPartLit literal

data UrlBuilder = UrlBuilder [UrlPart] [UrlQueryPart]


data ResponseT  = ResponseT String
                | ResponseTArray String

type MethodName = String

data MethodInfo = MethodInfo MethodName ResponseT [MyField]

data MethodTryTo= MethodTryToGet MethodInfo UrlBuilder
                | MethodTryToPost MethodInfo String UrlBuilder

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

        methodArgs = mkArgs args
        body  = mkTemplateSimplePostMethodBody methodName responseTA dataT args urlGet
        mkTemplateSimplePostMethodBody methodName responseTTypeArgument dataT args urlGet = (mkUrlBuilder urlGet ++ [dataC, ret])
            where 
                dataC = mkData methodName args
                ret = mkReturnServerConfig "TryToPost" [(mkTypeNamedTypeArgument dataT), responseTTypeArgument] [mkSimpleNameArgument "urlBuilder", mkSimpleNameArgument "data"]
                mkData methodName args = mkAndInitLocalVar "data" $ mkInvocation (map mkArg args)
                    where
                        mkArg (IntField n) = mkSimpleNameArgument  n
                        mkArg (StringField n) = mkSimpleNameArgument n
                        mkArg (StringNotEmptyField n) = mkSimpleNameArgument n
                        mkArg (StringNotEmptyArrayField n) = mkSimpleNameArgument n
                        mkArg (DateTimeField n) = mkSimpleNameArgument n
                        mkArg (DateTimeNullableField n) = mkSimpleNameArgument n
                        mkArg (CustomField t n) = mkSimpleNameArgument n
                        mkInvocation args = mkInvocationSimpleName (methodName ++ "MapData") args   
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
        urlPartAsArgument (UrlPartVar (IntField n)) = mkSimpleNameArgument  n
        urlPartAsArgument (UrlPartVar (StringField n)) = mkSimpleNameArgument n
        urlPartAsArgument (UrlPartVar (StringNotEmptyField n)) = mkSimpleNameArgument (n ++ ".Value")
        urlPartAsArgument (UrlPartVar (StringNotEmptyArrayField n)) = error "StringNotEmptyArrayField Not supported in url part"
        urlPartAsArgument (UrlPartVar (DateTimeField n)) = mkSimpleNameArgument ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
        urlPartAsArgument (UrlPartVar (DateTimeNullableField n)) = mkSimpleNameArgument n
        urlPartAsArgument (UrlPartVar (CustomField t n)) = error "CustomField Not supported in url part"

        urlQueryPartValueAsArgument (UrlQueryPartLit v) = mkLiteralStringArgument v
        urlQueryPartValueAsArgument (UrlQueryPartVar (IntField n)) = mkSimpleNameArgument (n ++ ".ToString()")
        urlQueryPartValueAsArgument (UrlQueryPartVar (StringField n)) = mkSimpleNameArgument n
        urlQueryPartValueAsArgument (UrlQueryPartVar (StringNotEmptyField n)) = mkSimpleNameArgument (n ++ ".Value")
        urlQueryPartValueAsArgument (UrlQueryPartVar (StringNotEmptyArrayField n)) = mkSimpleNameArgument n
        urlQueryPartValueAsArgument (UrlQueryPartVar (DateTimeField n)) = mkSimpleNameArgument ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ")")
        urlQueryPartValueAsArgument (UrlQueryPartVar (DateTimeNullableField n)) = mkSimpleNameArgument ("HttpUtils.DateTimeZoneHandlingUtcIso8601(" ++ n ++ ".Value)")
        urlQueryPartValueAsArgument (UrlQueryPartVar (CustomField _ n)) = error "CustomField Not supported in url query part"

        urlQueryPartAsArgument (UrlQueryPart n v) = mkNewArgument "UrlQueryParameter" $ [mkLiteralStringArgument n, urlQueryPartValueAsArgument v]
