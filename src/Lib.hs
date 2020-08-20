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
    createAndWriteToFileTemplateSimpleGet createTasksServerRequestsFile
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
        (MethodInfo "TryToGetDeleteFutureDatesRequest" (ResponseT "Response") [itemIdField, CustomField "FutureDatesToDeleteDto" "datesToDelete"])
        "FutureDatesToDeleteDto"
        (UrlBuilder [UrlPartLit "expirations", UrlPartVar (StringField "itemId + \":deletebatch\"")] [])
    ]

createTasksServerRequestsFile = 
    let 
        offsetField = IntField "offset"
        offsetQueryPart = mkUrlQueryPartVar "offset" offsetField 
        limitField = IntField "limit"
        limitQueryPart = mkUrlQueryPartVar "limit" limitField
        statusField = StringNotEmptyArrayField "status"
        statusQueryPart = mkUrlQueryPartVar "status" statusField
        typesOfTasksField = StringNotEmptyArrayField "typesOfTasks"
        typesOfTasksQueryPart =mkUrlQueryPartVar "type" typesOfTasksField 
        searchField = StringField "search"
        searchQueryPart = mkUrlQueryPartVar "search" searchField
        parentTaskIdField = StringField "parentTaskId"
        parentTaskIdQueryPart = mkUrlQueryPartVar "parent" parentTaskIdField
        fromScheduledStartField = DateTimeNullableField "fromScheduledStart"
        fromScheduledStartQueryPart = mkUrlQueryPartVar "from_scheduled_start" fromScheduledStartField 
        toScheduledStartField = DateTimeNullableField "toScheduledStart"
        toScheduledStartQueryPart = mkUrlQueryPartVar "to_scheduled_start" toScheduledStartField
    in TemplateSimpleGet 
    [ "Tlantic.Functional", "Tlantic.Server.Internal.Dtos"]
    "Tasks" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetTasksInstoreAdapterRequest" (ResponseTArray "TasksInstoreAdapterResponse") [ offsetField, limitField
            , retailStoreIdField, statusField, typesOfTasksField, searchField, parentTaskIdField, fromScheduledStartField, toScheduledStartField]
        )
        (UrlBuilder [UrlPartLit "tasks"] [ mkUrlQueryPartLiteral "adapter" "instore-adapter", offsetQueryPart, limitQueryPart, retailStoreIdQueryPart
            , statusQueryPart, typesOfTasksQueryPart, searchQueryPart, parentTaskIdQueryPart, fromScheduledStartQueryPart, toScheduledStartQueryPart]
        )
    , MethodTryToGet 
        (MethodInfo "TryToGetTasksSummaryRequest" (ResponseT "TasksSummaryResponse") [ retailStoreIdField, statusField, fromScheduledStartField, toScheduledStartField])
        (UrlBuilder [UrlPartLit "tasks-summary"] [ retailStoreIdQueryPart, statusQueryPart, fromScheduledStartQueryPart, toScheduledStartQueryPart]) 
    , MethodTryToPost 
        (MethodInfo "TryToGetCreateTaskRequest" (ResponseT "Response<CreateTaskResponse>") [CustomField "CreateTaskRequest" "createTaskRequest"])
        "CreateTaskRequestToSend"
        (UrlBuilder [UrlPartLit "tasks"] []) 
    ]
