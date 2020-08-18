module Lib
    ( someFunc
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import TemplateSimpleGet

someFunc :: IO ()
someFunc = do
    createAndWriteToFileTemplateSimpleGet createBarcodesServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createChecklistsServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createPrintersServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createDocumentsServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createItemListsServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createProductsServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createHierarchicalStructureServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createResourcesServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createTasksServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createUsersServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createExpirationsServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createTaskIntegrationsServerRequestsFile
    createAndWriteToFileTemplateSimpleGet createSettingsServerRequestsFile
    
retailStoreIdNotEmptyField = StringNotEmptyField "retailStoreId"
retailStoreIdNotEmptyQueryPart = mkUrlQueryPart "store" retailStoreIdNotEmptyField
retailStoreIdField = StringField "retailStoreId"
retailStoreIdQueryPart = mkUrlQueryPart "store" retailStoreIdField
itemIdField = StringField "itemId"

createSettingsServerRequestsFile = TemplateSimpleGet 
    [ mkUsing "System.Collections.Generic"
    , mkUsing "Tlantic.Server.Internal.Dtos"] 
    "Settings" 
    [ MethodTryToPost 
        (MethodInfo "TryToGetGetSettingsForCodesRequestServer" (ResponseT "Response<List<Setting>>") [CustomField "string[]" "settingCodes"])
        "SettingsRequest"
        (UrlGet [UrlPartLit "settings/entity/application"] [])
    ]

createTaskIntegrationsServerRequestsFile = TemplateSimpleGet 
    [ mkUsing "Tlantic.Functional"
    , mkUsing "System.Collections.Generic"
    , mkUsing "Tlantic.Server.Internal.Dtos"] 
    "TaskIntegrations" 
    [ MethodTryToPost 
        (MethodInfo "TryToGetCreateNewFutureDateByIntegrationRequest" (ResponseT "Response") [retailStoreIdNotEmptyField, CustomField "List<NewFutureDateByIntegrationRequestResource>" "resources"])
        "NewFutureDateByIntegrationRequest"
        (UrlGet [UrlPartLit "task-integrations"] [])
    , MethodTryToPost 
        (MethodInfo "TryToGetCreateTaskAsyncRequest" (ResponseT "Response") [CustomField "CreateTaskAsyncRequest" "createTaskAsyncRequest"])
        "CreateTaskAsyncRequestToSend"
        (UrlGet [UrlPartLit "task-integrations"] [])
    ]

createExpirationsServerRequestsFile = TemplateSimpleGet 
    [] 
    "Expirations" 
    [ MethodTryToPost 
        (MethodInfo "TryToGetDeleteFutureDatesRequest" (ResponseT "Response") [itemIdField, CustomField "FutureDatesToDeleteDto" "datesToDelete"])
        "FutureDatesToDeleteDto"
        (UrlGet [UrlPartLit "expirations", UrlPartVar (StringField "itemId + \":deletebatch\"")] [])
    ]

createUsersServerRequestsFile = TemplateSimpleGet 
    [mkUsing "System.Collections.Generic"] 
    "Users" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetUserInfo" (ResponseT "UserInfo") [])
        (UrlGet [UrlPartLit "users/user"] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetUserStoresRequest" (ResponseT "List<Store>") [])
        (UrlGet [UrlPartLit "user/stores"] [])
    ]

createTasksServerRequestsFile = 
    let 
        offsetField = IntField "offset"
        offsetQueryPart = mkUrlQueryPart "offset" offsetField 
        limitField = IntField "limit"
        limitQueryPart = mkUrlQueryPart "limit" limitField
        statusField = StringNotEmptyArrayField "status"
        statusQueryPart = mkUrlQueryPart "status" statusField
        typesOfTasksField = StringNotEmptyArrayField "typesOfTasks"
        typesOfTasksQueryPart =mkUrlQueryPart "type" typesOfTasksField 
        searchField = StringField "search"
        searchQueryPart = mkUrlQueryPart "search" searchField
        parentTaskIdField = StringField "parentTaskId"
        parentTaskIdQueryPart = mkUrlQueryPart "parent" parentTaskIdField
        fromScheduledStartField = DateTimeNullableField "fromScheduledStart"
        fromScheduledStartQueryPart = mkUrlQueryPart "from_scheduled_start" fromScheduledStartField 
        toScheduledStartField = DateTimeNullableField "toScheduledStart"
        toScheduledStartQueryPart = mkUrlQueryPart "to_scheduled_start" toScheduledStartField
    in TemplateSimpleGet 
    [ mkUsing "Tlantic.Functional"
    , mkUsing "Tlantic.Server.Internal.Dtos"]
    "Tasks" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetTasksInstoreAdapterRequest" (ResponseTArray "TasksInstoreAdapterResponse") [ offsetField, limitField
            , retailStoreIdField, statusField, typesOfTasksField, searchField, parentTaskIdField, fromScheduledStartField, toScheduledStartField]
        )
        (UrlGet [UrlPartLit "tasks"] [ UrlQueryPart "adapter" "\"instore-adapter\"", offsetQueryPart, limitQueryPart, retailStoreIdQueryPart
            , statusQueryPart, typesOfTasksQueryPart, searchQueryPart, parentTaskIdQueryPart, fromScheduledStartQueryPart, toScheduledStartQueryPart]
        )
    , MethodTryToGet 
        (MethodInfo "TryToGetTasksSummaryRequest" (ResponseT "TasksSummaryResponse") [ retailStoreIdField, statusField, fromScheduledStartField, toScheduledStartField])
        (UrlGet [UrlPartLit "tasks-summary"] [ retailStoreIdQueryPart, statusQueryPart, fromScheduledStartQueryPart, toScheduledStartQueryPart]) 
    , MethodTryToPost 
        (MethodInfo "TryToGetCreateTaskRequest" (ResponseT "Response<CreateTaskResponse>") [CustomField "CreateTaskRequest" "createTaskRequest"])
        "CreateTaskRequestToSend"
        (UrlGet [UrlPartLit "tasks"] []) 
    ]

createResourcesServerRequestsFile = 
    let 
        itemIdField = StringField "itemId"
    in TemplateSimpleGet 
    [mkUsing "Tlantic.Functional"]
    "Resources" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetFutureDatesRequest" (ResponseT "FutureDatesDto") [itemIdField, retailStoreIdNotEmptyField])
        (UrlGet [UrlPartLit "resources/items/sku", UrlPartVar itemIdField, UrlPartLit "expirations"] [retailStoreIdNotEmptyQueryPart]) 
    ]

createHierarchicalStructureServerRequestsFile = 
    let 
        rootHsIdField = StringField "rootHsId"
    in TemplateSimpleGet 
    []
    "HierarchicalStructure" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetHierarchicalStructureRequest" (ResponseTArray "HierarchicalStructureEntry") [rootHsIdField])
        (UrlGet [UrlPartLit "hierarchicalstructure", UrlPartVar rootHsIdField] [])
    ]

createProductsServerRequestsFile = 
    let 
        itemIdField = StringField "itemId"
        itemIdNotEmptyField = StringNotEmptyField "itemId"
        eanField = StringField "ean"
        eanQueryPart = mkUrlQueryPart "ean" eanField
        dateField = DateTimeField "date"
        chainField = StringNotEmptyField "chain"
        expirationDateField = DateTimeField "expirationDate"
        expirationDateQueryPart = mkUrlQueryPart "date" expirationDateField
    in TemplateSimpleGet 
    [mkUsing "Tlantic.Functional"]
    "Products" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetExpirationsItemParametersRequest" (ResponseT "ExpirationsItemParameters") [itemIdField, retailStoreIdNotEmptyField])
        (UrlGet [UrlPartLit "products", UrlPartVar itemIdField, UrlPartLit "expirations/parameters"] [retailStoreIdNotEmptyQueryPart]) 
    , MethodTryToGet 
        (MethodInfo "TryToGetDailySalesServerRequest" (ResponseT "Sales") [retailStoreIdNotEmptyField, eanField, dateField])
        (UrlGet [UrlPartLit "products/eans", UrlPartVar eanField, UrlPartLit "sales", UrlPartVar retailStoreIdNotEmptyField, UrlPartVar dateField] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetDamagesDestinationsRequest" (ResponseTArray "DamagesDestinationsResponse") [retailStoreIdNotEmptyField])
        (UrlGet [UrlPartLit "products/damages", UrlPartVar retailStoreIdNotEmptyField, UrlPartLit "destinations"] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetDamagesReasonsRequest" (ResponseTArray "DamagesReasonsResponse") []) 
        (UrlGet [UrlPartLit "products/damages/reasons"] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetExternalPVPRequest" (ResponseT "PriceValue") [ itemIdField, retailStoreIdNotEmptyField])
        (UrlGet [UrlPartLit "products", UrlPartVar itemIdField, UrlPartLit "price"] [retailStoreIdNotEmptyQueryPart]) 
    , MethodTryToGet 
        (MethodInfo "TryToGetPricesRequest" (ResponseTArray "PriceResponse") [retailStoreIdNotEmptyField, itemIdNotEmptyField, eanField])
        (UrlGet [UrlPartLit "products", UrlPartVar itemIdNotEmptyField, UrlPartLit "prices"] [retailStoreIdNotEmptyQueryPart, eanQueryPart]) 
    , MethodTryToGet 
        (MethodInfo "TryToGetOtherStoresStockRequest" (ResponseTArray "OtherStoresStockServerResponse") [itemIdNotEmptyField, chainField])
        (UrlGet [UrlPartLit "products", UrlPartVar itemIdNotEmptyField, UrlPartLit "stock/chain", UrlPartVar chainField] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetWithdrawalExternalInfoRequest" (ResponseT "WithdrawalExternalInfo") [itemIdField, expirationDateField, retailStoreIdNotEmptyField])
        (UrlGet [UrlPartLit "products", UrlPartVar itemIdField, UrlPartLit "expirations/quantities"] [retailStoreIdNotEmptyQueryPart, expirationDateQueryPart])
    , MethodTryToPost
        (MethodInfo "TryToGetSendFutureValiditiesRequest" (ResponseT "Response") [itemIdNotEmptyField, CustomField "SendFutureValiditiesRequest" "sendFutureValiditiesRequest"])
        "SendFutureValiditiesRequestToSend"
        (UrlGet [UrlPartLit "products", UrlPartVar itemIdNotEmptyField, UrlPartLit "validities"] [])
    ]

createItemListsServerRequestsFile = TemplateSimpleGet 
    []
    "ItemLists" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetItemListsRequest" (ResponseTArray "ItemListEntryResponse") []) 
        (UrlGet [UrlPartLit "item-lists"] [])
    ]

createDocumentsServerRequestsFile = TemplateSimpleGet 
    []
    "Documents" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetGetAvailableLabelsRequest" (ResponseTArray "DocumentEntryResponse") [retailStoreIdField])
        (UrlGet [UrlPartLit "documents"] [retailStoreIdQueryPart]) 
    ]

createPrintersServerRequestsFile = 
    let 
        macAddressField = StringNotEmptyField "macAddress"
    in TemplateSimpleGet 
    [mkUsing "Tlantic.Functional"]
    "Printers" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetPrinterRequest" (ResponseT "PrinterEntryReponse") [macAddressField])
        (UrlGet [UrlPartLit "printers", UrlPartVar macAddressField] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetPrintersRequest" (ResponseTArray "PrinterEntryReponse") [retailStoreIdField])
        (UrlGet [UrlPartLit "printers"] [retailStoreIdQueryPart])
    ]

createChecklistsServerRequestsFile = TemplateSimpleGet 
    []
    "Checklists" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetChecklistsRequest" (ResponseTArray "ChecklistEntryResponse") [])
        (UrlGet [UrlPartLit "checklists"] [])
    ]
    
createBarcodesServerRequestsFile = TemplateSimpleGet 
    []
    "Barcodes" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetScanCodeRulesRequest" (ResponseTArray "BarcodeRule") [])
        (UrlGet [UrlPartLit "barcodes/rules"] [])
    ]
    