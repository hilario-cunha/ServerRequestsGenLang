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

data TemplateSimpleGet = TemplateSimpleGet [String] String [MethodTryTo]
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
            (map mkTemplateSimpleMethod methodsTryTo)
        )
    where 
        usingsAux = ("Tlantic.Server.Core" : "System" : extraUsings)
        namespace = ("Tlantic.Server." ++ functionalityName)
        cn = functionalityName ++ "ServerRequests"
        mkTemplateSimpleMethod (MethodTryToGet mi u) = mkTemplateSimpleGetMethod mi u
        mkTemplateSimpleMethod (MethodTryToPost mi dataT u) = mkTemplateSimplePostMethod mi dataT u

mkTemplateSimpleGetMethodBody :: UrlBuilder -> TypeArgument -> [Statement]
mkTemplateSimpleGetMethodBody urlGet innerResponseTA = (mkUrlBuilder urlGet ++ [returnStatement])
    where
        returnStatement = mkReturn $ Invocation tryToGetMemberAccess trytoGetArgs
        trytoGetArgs = [mkSimpleNameArgument "urlBuilder"]
        tryToGetMemberAccess = MemberAccess $ mkPrimaryMemberAccessWithTypeArguments (mkSimpleName "serverConfig") "TryToGet" [innerResponseTA]

mkTemplateSimpleGetMethod :: MethodInfo -> UrlBuilder -> MemberDeclaration
mkTemplateSimpleGetMethod (MethodInfo methodName responseT args) urlGet = 
    mkMethodMemberDeclaration [Public] returnType methodName (mkArgs args) (mkTemplateSimpleGetMethodBody urlGet innerResponseTA)
    where 
        returnType = mkTypeNamedWithTypeArguments "IChoiceGetRequestWithRetry" [responseTA, networkErrorTA]
        responseTA = TypeArgument (mkTypeNamedWithTypeArguments "Response" [innerResponseTA])
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")
        innerResponseTA = mkInnerResponseTA responseT

mkTemplateSimplePostMethod :: MethodInfo -> [Char] -> UrlBuilder -> MemberDeclaration
mkTemplateSimplePostMethod (MethodInfo methodName responseT args) dataT urlGet = mkMethodMemberDeclaration [Public] methodReturnT methodName methodArgs body
    where 
        methodReturnT = (mkTypeNamedWithTypeArguments "IChoicePostRequestWithRetry" [responseTA, networkErrorTA])
        responseTA = mkInnerResponseTA responseT
        networkErrorTA = TypeArgument  (mkTypeNamed "NetworkError")

        methodArgs = mkArgs (args ++ [CustomField dataT "data"])
        body  = mkTemplateSimplePostMethodBody responseTA
        mkTemplateSimplePostMethodBody responseTTypeArgument = (mkUrlBuilder urlGet ++ [ret])
            where 
                ret = mkReturnServerConfig "TryToPost" [(mkTypeNamedTypeArgument dataT), responseTTypeArgument] [mkSimpleNameArgument "urlBuilder", mkSimpleNameArgument "data"]
                mkReturnServerConfig mn tArgs args1 = Return (Just (Invocation (MemberAccess $ PrimaryMemberAccess (SimpleName (Identifier "serverConfig") [] ) (Identifier mn) tArgs) args1))

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
