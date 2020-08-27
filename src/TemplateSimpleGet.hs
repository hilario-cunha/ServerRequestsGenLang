module TemplateSimpleGet
    ( TemplateSimpleGet(..)
    , createAndWriteToFileTemplateSimpleGet
    , MethodInfo(..)
    , MethodTryTo(..)
    , MyField(..)
    , MethodAction(..)
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

data ResponseT  = Response
                | ResponseT String
                | ResponseTArray String
                deriving Show

type MethodName = String

data MethodInfo = MethodInfo MethodName ResponseT [MyField]
    deriving Show

data MethodAction = MethodActionGet | MethodActionPost String
    deriving Show

data MethodTryTo = MethodTryTo MethodAction MethodInfo UrlBuilder
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
    
serverConfigDot :: String -> [TypeArgument] -> [Argument] -> Expression
serverConfigDot methodName typeArguments  = Invocation $ MemberAccess $ mkPrimaryMemberAccessWithTypeArguments (mkSimpleName "serverConfig") methodName typeArguments 

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
            ((mkTemplateSimpleMethods methodsTryTo) ++ urlBuilderMethods)
        )
    where 
        extractUrlBuilderMethod (MethodTryTo _ (MethodInfo methodName _ _) urlBuilder) = UlrBuilderMethod ("CreateUrlBuilder" ++ methodName) urlBuilder
        
        extractUrlBuilderMethods = map extractUrlBuilderMethod methodsTryTo
        urlBuilderMethods = generateUrlBuilderMethods extractUrlBuilderMethods

        usingsAux = ("Tlantic.Server.Core" : "System" : extraUsings)
        namespace = ("Tlantic.Server." ++ functionalityName)
        cn = functionalityName ++ "ServerRequests"

mkTemplateSimpleMethods :: [MethodTryTo] -> [MemberDeclaration]
mkTemplateSimpleMethods = map mkTemplateSimpleMethod

mkTemplateSimpleMethod :: MethodTryTo -> MemberDeclaration
mkTemplateSimpleMethod (MethodTryTo methodAction (MethodInfo methodName responseT args) u) = 
    mkMethodMemberDeclaration [Public] returnType methodName (mkArgs args) body
    where 
        responseTA = mkResponseTA responseT
        
        returnType = (mkIChoice methodAction)
        mkIChoice (MethodActionGet) = mkReturnType "IChoiceGetRequestWithRetry"
        mkIChoice (MethodActionPost _) = mkReturnType "IChoicePostRequestWithRetry"
        mkReturnType iChoiceType = mkTypeNamedWithTypeArguments iChoiceType [responseTA, TypeArgument (mkTypeNamed "NetworkError")]
        
        body = 
            [ callUrlBuilder $ UlrBuilderMethod ("CreateUrlBuilder" ++ methodName) u
            , callMethodAction methodAction
            ]
        
        -- return serverConfig.TryToGet<Response<BarcodeRule[]>>(urlBuilder);
        callMethodAction (MethodActionGet) =  mkReturn $ serverConfigDot "TryToGet" [responseTA] [mkSimpleNameArgument "urlBuilder"]
        -- return serverConfig.TryToPost<SendFutureValiditiesRequestToSend,Response>(urlBuilder,data);
        callMethodAction (MethodActionPost dataT) =  mkReturn $ serverConfigDot "TryToPost" [(mkTypeNamedTypeArgument dataT), responseTA] [mkSimpleNameArgument "urlBuilder", mkSimpleNameArgument "data"]


        
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
