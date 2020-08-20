module Lib
    ( someFunc
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import TemplateSimpleGet
import CSharpGen

someFunc :: IO ()
someFunc = do
    createAndWriteToFileTemplateSimpleGet createExpirationsServerRequestsFile
    
retailStoreIdNotEmptyField = StringNotEmptyField "retailStoreId"
retailStoreIdNotEmptyQueryPart = mkUrlQueryPartVar "store" retailStoreIdNotEmptyField
retailStoreIdField = StringField "retailStoreId"
retailStoreIdQueryPart = mkUrlQueryPartVar "store" retailStoreIdField
itemIdField = StringField "itemId"

createExpirationsServerRequestsFile = TemplateSimpleGet 
    [] 
    "Expirations" 
    [ MethodTryToPost 
        (MethodInfo "TryToGetDeleteFutureDatesRequest" (ResponseT "Response") [itemIdField])
        "FutureDatesToDeleteDto"
        (UrlBuilder [UrlPartLit "expirations", UrlPartVar (StringField "itemId + \":deletebatch\"")] [])
    ]
