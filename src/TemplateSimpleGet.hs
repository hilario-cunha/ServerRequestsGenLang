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
import Data.Maybe (fromJust)

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

data ResponseT  = Response
                | ResponseT String
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
    
callMethodFromServerConfig :: String -> [TypeArgument] -> [Argument] -> Expression
callMethodFromServerConfig methodName typeArguments  = Invocation $ MemberAccess $ mkPrimaryMemberAccessWithTypeArguments (mkSimpleName "serverConfig") methodName typeArguments 

generateUrlBuilderMethods :: [UlrBuilderMethod] -> [MemberDeclaration]
generateUrlBuilderMethods urlBuilderMethods = map generateUlrBuilderMethod urlBuilderMethods

callUrlBuilder :: UlrBuilderMethod -> Statement
callUrlBuilder ulrBuilderMethod = mkAndInitLocalVar "urlBuilder" $  invokeUrlBuilderMethod ulrBuilderMethod

createTemplateSimpleGet :: TemplateSimpleGet -> NamespaceWithClass
createTemplateSimpleGet (TemplateSimpleGet extraUsings functionalityName methodsTryTo) = 
    createNamespaceWithClass
        usingsAux 
        namespace
        (createClassWithMethods 
            cn
            (mkTemplateSimpleGetCtor cn)
            ((map mkTemplateSimpleMethod methodsTryTo) ++ urlBuilderMethods)
        )
    where 
        extractUrlBuilderMethod (MethodTryToGet (MethodInfo methodName _ _) urlBuilder) = UlrBuilderMethod ("CreateUrlBuilder" ++ methodName) urlBuilder
        extractUrlBuilderMethod (MethodTryToPost (MethodInfo methodName _ _) _ urlBuilder) = UlrBuilderMethod ("CreateUrlBuilder" ++ methodName) urlBuilder
        
        extractUrlBuilderMethods = map extractUrlBuilderMethod methodsTryTo
        urlBuilderMethods = generateUrlBuilderMethods extractUrlBuilderMethods

        usingsAux = ("Tlantic.Server.Core" : "System" : extraUsings)
        namespace = ("Tlantic.Server." ++ functionalityName)
        cn = functionalityName ++ "ServerRequests"

        mkReturnType iChoiceType responseT = mkTypeNamedWithTypeArguments iChoiceType [mkResponseTA responseT, TypeArgument (mkTypeNamed "NetworkError")]
        mkIChoiceGetRequestWithRetryType = mkReturnType "IChoiceGetRequestWithRetry" 
        mkIChoicePostRequestWithRetryType = mkReturnType "IChoicePostRequestWithRetry"

        mkTemplateSimpleMethod (MethodTryToGet (MethodInfo methodName responseT args) u) = 
            mkMethodMemberDeclaration [Public] (mkIChoiceGetRequestWithRetryType responseT) methodName (mkArgs args) [callUrlBuilderMethod, callTryToGet]
            where 
                ulrBuilderMethod = UlrBuilderMethod ("CreateUrlBuilder" ++ methodName) u
                callUrlBuilderMethod = callUrlBuilder ulrBuilderMethod

                innerResponseTA = fromJust $ mkInnerResponseTA responseT
                callTryToGet = mkReturn $ callMethodFromServerConfig "TryToGet" [innerResponseTA] [mkSimpleNameArgument "urlBuilder"]

        mkTemplateSimpleMethod (MethodTryToPost (MethodInfo methodName responseT args) dataT u) = 
            mkMethodMemberDeclaration [Public] (mkIChoicePostRequestWithRetryType responseT) methodName (mkArgs (args ++ [CustomField dataT "data"])) [callUrlBuilderMethod, callTryToPost]
            where 
                ulrBuilderMethod = UlrBuilderMethod ("CreateUrlBuilder" ++ methodName) u
                callUrlBuilderMethod = callUrlBuilder ulrBuilderMethod

                callTryToPost = mkReturn $ callMethodFromServerConfig "TryToPost" [(mkTypeNamedTypeArgument dataT), mkResponseTA responseT] [mkSimpleNameArgument "urlBuilder", mkSimpleNameArgument "data"]


mkResponseTA :: ResponseT -> TypeArgument
mkResponseTA responseT = maybe (mkTypeNamedTypeArgument "Response") (\innerResponseTA -> TypeArgument (mkTypeNamedWithTypeArguments "Response" [innerResponseTA])) $ mkInnerResponseTA responseT

mkInnerResponseTA :: ResponseT -> Maybe TypeArgument
mkInnerResponseTA (Response) = Nothing
mkInnerResponseTA (ResponseT t) = Just $ mkTypeNamedTypeArgument t
mkInnerResponseTA (ResponseTArray t) = Just $ mkTypeArrayTypeArgument t

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
