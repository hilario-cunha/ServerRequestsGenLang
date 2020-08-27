module TemplateSimpleGet
    ( TemplateSimpleGet(..)
    , createAndWriteToFileTemplateSimpleGet
    , MethodInfo(..)
    , MethodTryTo(..)
    , MyField(..)
    , mkFormalParamMyField
    , ResponseT(..)
    , extractNameFromMyField
    , extractFieldsFromMethodInfo
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import CSharpGen
import UrlBuilder

data MyField= IntField String
            | StringField String
            | StringNotEmptyField String
            | StringNotEmptyArrayField String
            | DateTimeField String
            | DateTimeNullableField String
            | CustomField String String
            deriving Show

data TemplateSimpleGet = TemplateSimpleGet [String] String [MethodTryTo]
    deriving Show

data ResponseT  = ResponseT String
                | ResponseTArray String
                deriving Show

type MethodName = String

data MethodInfo = MethodInfo MethodName ResponseT [MyField]
    deriving Show

data MethodTryTo= MethodTryToGet MethodInfo UrlBuilder
                | MethodTryToPost MethodInfo String UrlBuilder
                deriving Show

extractNameFromMyField :: MyField -> String
extractNameFromMyField (IntField n) = n
extractNameFromMyField (StringField n) = n
extractNameFromMyField (StringNotEmptyField n) = n
extractNameFromMyField (StringNotEmptyArrayField n) = n
extractNameFromMyField (DateTimeField n) = n
extractNameFromMyField (DateTimeNullableField n) = n
extractNameFromMyField (CustomField _ n) = n

mkFormalParamMyField :: MyField -> FormalParam
mkFormalParamMyField (IntField n) = mkFormalParam "int" n
mkFormalParamMyField (StringField n) = mkFormalParam "string" n
mkFormalParamMyField (StringNotEmptyField n) = mkFormalParam "StringNotEmpty" n
mkFormalParamMyField (StringNotEmptyArrayField n) = mkFormalParam "StringNotEmpty[]" n
mkFormalParamMyField (DateTimeField n) = mkFormalParam "DateTime" n
mkFormalParamMyField (DateTimeNullableField n) = mkFormalParam "DateTime?" n
mkFormalParamMyField (CustomField t n) = mkFormalParam t n

mkArgs :: [MyField] -> [FormalParam]
mkArgs fields = map mkFormalParamMyField fields

extractFieldsFromMethodInfo :: MethodInfo -> [MyField]
extractFieldsFromMethodInfo (MethodInfo _ _ fields) = fields

createAndWriteToFileTemplateSimpleGet :: TemplateSimpleGet -> IO ()
createAndWriteToFileTemplateSimpleGet templateData  = 
    let 
        createServerRequestsFile = createTemplateSimpleGet templateData
        fileName = "output/" ++ (className (classDefinition createServerRequestsFile)) ++ ".GenTemplateSimpleGet.cs"
        cu = mkCu createServerRequestsFile
    in createAndWriteToFile fileName cu

mkCu :: NamespaceWithClass -> CompilationUnit
mkCu = mkNamespaceWithClass mkTemplateSimpleGetClass
createAndWriteToFile :: Pretty a => FilePath -> a -> IO ()
createAndWriteToFile fileName cu  = writeFile fileName $ prettyPrint cu      
    
createTemplateSimpleGet :: TemplateSimpleGet -> NamespaceWithClass
createTemplateSimpleGet (TemplateSimpleGet extraUsings functionalityName methodsTryTo) = 
    createNamespaceWithClass
        usingsAux 
        namespace
        (createClassWithMethods 
            cn
            (mkTemplateSimpleGetCtor cn)
            (concat $ map mkTemplateSimpleMethod methodsTryTo)
        )
    where 
        usingsAux = ("Tlantic.Server.Core" : "System" : extraUsings)
        namespace = ("Tlantic.Server." ++ functionalityName)
        cn = functionalityName ++ "ServerRequests"
        mkTemplateSimpleMethod (MethodTryToGet mi@(MethodInfo mn _ _) u) = [generateUlrBuilderMethod ulrBuilderMethod, mkTemplateSimpleGetMethod mi ulrBuilderMethod]
            where 
                ulrBuilderMethod = UlrBuilderMethod ("CreateUrlBuilder" ++ mn) u
        mkTemplateSimpleMethod (MethodTryToPost mi@(MethodInfo mn _ _) dataT u) = [generateUlrBuilderMethod ulrBuilderMethod, mkTemplateSimplePostMethod mi dataT ulrBuilderMethod]
            where 
                ulrBuilderMethod = UlrBuilderMethod ("CreateUrlBuilder" ++ mn) u

callUrlBuilder :: UlrBuilderMethod -> Statement
callUrlBuilder ulrBuilderMethod = mkAndInitLocalVar "urlBuilder" $  invokeUrlBuilderMethod ulrBuilderMethod

mkTemplateSimpleGetMethod :: MethodInfo -> UlrBuilderMethod -> MemberDeclaration
mkTemplateSimpleGetMethod (MethodInfo methodName responseT args) ulrBuilderMethod = 
    mkMethodMemberDeclaration [Public] returnType methodName methodArgs [callUrlBuilder ulrBuilderMethod, callTryToGet]
    where 
        returnType = mkTypeNamedWithTypeArguments "IChoiceGetRequestWithRetry" [responseTA, networkErrorTA]
        responseTA = TypeArgument (mkTypeNamedWithTypeArguments "Response" [innerResponseTA])
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")
        innerResponseTA = mkInnerResponseTA responseT
        
        methodArgs = (mkArgs args)

        callTryToGet = mkReturn $ callMethodFromServerConfig "TryToGet" [innerResponseTA] [mkSimpleNameArgument "urlBuilder"]
        
callMethodFromServerConfig :: String -> [TypeArgument] -> [Argument] -> Expression
callMethodFromServerConfig methodName typeArguments  = Invocation $ MemberAccess $ mkPrimaryMemberAccessWithTypeArguments (mkSimpleName "serverConfig") methodName typeArguments 

mkTemplateSimplePostMethod :: MethodInfo -> String -> UlrBuilderMethod -> MemberDeclaration
mkTemplateSimplePostMethod (MethodInfo methodName responseT args) dataT ulrBuilderMethod = 
    mkMethodMemberDeclaration [Public] returnType methodName methodArgs [callUrlBuilder ulrBuilderMethod, callTryToPost]
    where 
        returnType = (mkTypeNamedWithTypeArguments "IChoicePostRequestWithRetry" [responseTA, networkErrorTA])
        responseTA = innerResponseTA
        innerResponseTA = mkInnerResponseTA responseT
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")

        methodArgs = mkArgs (args ++ [CustomField dataT "data"])

        callTryToPost = mkReturn $ callMethodFromServerConfig "TryToPost" [(mkTypeNamedTypeArgument dataT), innerResponseTA] [mkSimpleNameArgument "urlBuilder", mkSimpleNameArgument "data"]

mkTemplateSimpleGetClass :: ClassWithMethods -> Declaration
mkTemplateSimpleGetClass classWithMethods = 
    mkPublicClass cn cb
    where 
        cn = className classWithMethods
        cb = mkTemplateSimpleGetClassBody cn classWithMethods

mkTemplateSimpleGetClassBody :: [Char] -> ClassWithMethods -> [MemberDeclaration]
mkTemplateSimpleGetClassBody ctorName classWithMethods = (mkServerConfigField : ctor1 : ms)
    where 
        mkServerConfigField = mkField "ServerConfig" "serverConfig"
        ctor1 = mkTemplateSimpleGetCtor ctorName
        ms = methods classWithMethods

mkTemplateSimpleGetCtor :: String -> MemberDeclaration
mkTemplateSimpleGetCtor ctorName = mkConstructorMemberDeclaration [Internal] ctorName [serverConfigFormalParam] [serverConfigAssign]
    where
        serverConfigFormalParam = mkFormalParam "ServerConfig" "serverConfig"
        serverConfigAssign = mkAssignStatement "this.serverConfig" "serverConfig"

        
mkInnerResponseTA :: ResponseT -> TypeArgument
mkInnerResponseTA (ResponseT t) = mkTypeNamedTypeArgument t
mkInnerResponseTA (ResponseTArray t) = mkTypeArrayTypeArgument t
