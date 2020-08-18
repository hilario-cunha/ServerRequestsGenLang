module Gen
    ( mkTypeArray
    , mkTypeArrayTypeArgument
    , mkUsing
    , mkNamespace
    , mkClass
    , mkCtor
    , mkFormalParam
    , mkAssign
    , mkField
    , mkMethodPublic
    , mkTypeNamedArgs
    , mkTypeNamed
    , mkTypeNamedTypeArgument
    , mkAndInitLocalVar
    , mkStringLit
    , mkInvocationWithGenericTypes
    , mkSimpleNameArgument
    , NamespaceWithClass(..)
    , ClassWithMethods(..)
    , createNamespaceWithClass
    , createClassWithMethods
    , mkNamespaceWithClass
    , mkStringLitArgument
    , mkNew
    , mkNewArgument
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty

data NamespaceWithClass = NamespaceWithClass
    { usings :: [Using]
    , nameSpace :: String
    , classDefinition :: ClassWithMethods
    }

data ClassWithMethods = ClassWithMethods
    { className :: String
    , ctor :: MemberDeclaration
    , methods :: [MemberDeclaration]
    }

createNamespaceWithClass u n c =  NamespaceWithClass
    { usings = u
    , nameSpace = n
    , classDefinition = c
    }

createClassWithMethods cn c m = ClassWithMethods
    { className = cn
    , ctor = c
    , methods = m
    }

mkNamespaceWithClass createClassFromDefinition namespaceWithClass = 
    CompilationUnit 
        (usings namespaceWithClass) 
        [ mkNamespace (nameSpace namespaceWithClass) $ createClassFromDefinition (classDefinition namespaceWithClass)]

mkSimpleNameArgument n = Argument Nothing (SimpleName (Identifier n) [])
mkMethodPublic methodReturnType methodName methodArgs mb = MethodMemberDeclaration [] [Public] methodReturnType (mkName methodName) [] (FormalParams methodArgs Nothing) [] mb
mkAndInitLocalVar varName exp = Declaration (
    LocalVarDeclaration 
        (VarType (mkTypeNamed "var")) 
        [VariableDeclarator (Identifier varName) (Just (VariableInitializerExpression exp))]
    )
mkStringLitArgument str = Argument Nothing (mkStringLit str)
mkStringLit str = Literal (StringLit str)
mkTypeArrayTypeArgument t = (TypeArgument (mkTypeArray t))
mkTypeArray t = TypeArray (ArrayType (mkTypeNamed t) [RankSpecifier 0])
mkPrimaryMemberAccess vn n ts = MemberAccess (PrimaryMemberAccess (SimpleName (Identifier vn) []) (Identifier n) ts)
mkInvocationWithGenericTypes vn n ts args = Invocation (mkPrimaryMemberAccess vn n ts) args   
mkAssign l r = ExpressionStatement $ Assign (SimpleName (Identifier l) []) OpAssign (SimpleName (Identifier r) [])
mkCtor className formalParams cs = ConstructorMemberDeclaration [] [Internal] (Identifier className) (FormalParams formalParams (Nothing)) Nothing (ConstructorStatementBody cs)
mkFormalParam t n = FormalParam (Nothing) (mkTypeNamed t) (Identifier n) (Nothing)
mkName n = (Name [Identifier n])
mkNamespace namespace classDefinition = NamespaceDeclaration [] (mkName namespace) [classDefinition]
mkClass className cb = TypeDeclaration $ ClassTypeDeclaration [] [Public, Partial] (Identifier className) [] [] [] (ClassBody cb)
mkVariableDeclarator n = VariableDeclarator (Identifier n) (Nothing)
mkField t n = FieldMemberDeclaration [] [] (mkTypeNamed t) [mkVariableDeclarator n]
mkTypeNamedArgs t args = (TypeNamed (TypeName (mkName t) args))
mkTypeNamedTypeArgument t = TypeArgument (mkTypeNamed t)
mkTypeNamed t = (TypeNamed (TypeName (mkName t) []))
mkUsing namespace = Using (mkName namespace) False
mkNew cn args = ObjectCreationExpression (TypeNamed (TypeName (Name [Identifier cn]) [])) args Nothing
mkNewArgument cn args = Argument Nothing (mkNew cn args)
