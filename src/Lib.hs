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
retailStoreIdNotEmptyQueryPart = mkUrlQueryPartVar "store" retailStoreIdNotEmptyField
retailStoreIdField = StringField "retailStoreId"
retailStoreIdQueryPart = mkUrlQueryPartVar "store" retailStoreIdField
itemIdField = StringField "itemId"

createSettingsServerRequestsFile = TemplateSimpleGet 
    [ "System.Collections.Generic", "Tlantic.Server.Internal.Dtos"] 
    "Settings" 
    [ MethodTryToPost 
        (MethodInfo "TryToGetGetSettingsForCodesRequestServer" (ResponseT "Response<List<Setting>>") [CustomField "string[]" "settingCodes"])
        "SettingsRequest"
        (UrlBuilder [UrlPartLit "settings/entity/application"] [])
    ]

createTaskIntegrationsServerRequestsFile = TemplateSimpleGet 
    [ "Tlantic.Functional", "System.Collections.Generic", "Tlantic.Server.Internal.Dtos"] 
    "TaskIntegrations" 
    [ MethodTryToPost 
        (MethodInfo "TryToGetCreateNewFutureDateByIntegrationRequest" (ResponseT "Response") [retailStoreIdNotEmptyField, CustomField "List<NewFutureDateByIntegrationRequestResource>" "resources"])
        "NewFutureDateByIntegrationRequest"
        (UrlBuilder [UrlPartLit "task-integrations"] [])
    , MethodTryToPost 
        (MethodInfo "TryToGetCreateTaskAsyncRequest" (ResponseT "Response") [CustomField "CreateTaskAsyncRequest" "createTaskAsyncRequest"])
        "CreateTaskAsyncRequestToSend"
        (UrlBuilder [UrlPartLit "task-integrations"] [])
    ]

createExpirationsServerRequestsFile = TemplateSimpleGet 
    [] 
    "Expirations" 
    [ MethodTryToPost 
        (MethodInfo "TryToGetDeleteFutureDatesRequest" (ResponseT "Response") [itemIdField, CustomField "FutureDatesToDeleteDto" "datesToDelete"])
        "FutureDatesToDeleteDto"
        (UrlBuilder [UrlPartLit "expirations", UrlPartVar (StringField "itemId + \":deletebatch\"")] [])
    ]

createUsersServerRequestsFile = TemplateSimpleGet 
    ["System.Collections.Generic"] 
    "Users" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetUserInfo" (ResponseT "UserInfo") [])
        (UrlBuilder [UrlPartLit "users/user"] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetUserStoresRequest" (ResponseT "List<Store>") [])
        (UrlBuilder [UrlPartLit "user/stores"] [])
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

createResourcesServerRequestsFile = 
    let 
        itemIdField = StringField "itemId"
    in TemplateSimpleGet 
    ["Tlantic.Functional"]
    "Resources" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetFutureDatesRequest" (ResponseT "FutureDatesDto") [itemIdField, retailStoreIdNotEmptyField])
        (UrlBuilder [UrlPartLit "resources/items/sku", UrlPartVar itemIdField, UrlPartLit "expirations"] [retailStoreIdNotEmptyQueryPart]) 
    ]

createHierarchicalStructureServerRequestsFile = 
    let 
        rootHsIdField = StringField "rootHsId"
    in TemplateSimpleGet 
    []
    "HierarchicalStructure" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetHierarchicalStructureRequest" (ResponseTArray "HierarchicalStructureEntry") [rootHsIdField])
        (UrlBuilder [UrlPartLit "hierarchicalstructure", UrlPartVar rootHsIdField] [])
    ]

createProductsServerRequestsFile = 
    let 
        itemIdField = StringField "itemId"
        itemIdNotEmptyField = StringNotEmptyField "itemId"
        eanField = StringField "ean"
        eanQueryPart = mkUrlQueryPartVar "ean" eanField
        dateField = DateTimeField "date"
        chainField = StringNotEmptyField "chain"
        expirationDateField = DateTimeField "expirationDate"
        expirationDateQueryPart = mkUrlQueryPartVar "date" expirationDateField
    in TemplateSimpleGet 
    ["Tlantic.Functional"]
    "Products" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetExpirationsItemParametersRequest" (ResponseT "ExpirationsItemParameters") [itemIdField, retailStoreIdNotEmptyField])
        (UrlBuilder [UrlPartLit "products", UrlPartVar itemIdField, UrlPartLit "expirations/parameters"] [retailStoreIdNotEmptyQueryPart]) 
    , MethodTryToGet 
        (MethodInfo "TryToGetDailySalesServerRequest" (ResponseT "Sales") [retailStoreIdNotEmptyField, eanField, dateField])
        (UrlBuilder [UrlPartLit "products/eans", UrlPartVar eanField, UrlPartLit "sales", UrlPartVar retailStoreIdNotEmptyField, UrlPartVar dateField] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetDamagesDestinationsRequest" (ResponseTArray "DamagesDestinationsResponse") [retailStoreIdNotEmptyField])
        (UrlBuilder [UrlPartLit "products/damages", UrlPartVar retailStoreIdNotEmptyField, UrlPartLit "destinations"] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetDamagesReasonsRequest" (ResponseTArray "DamagesReasonsResponse") []) 
        (UrlBuilder [UrlPartLit "products/damages/reasons"] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetExternalPVPRequest" (ResponseT "PriceValue") [ itemIdField, retailStoreIdNotEmptyField])
        (UrlBuilder [UrlPartLit "products", UrlPartVar itemIdField, UrlPartLit "price"] [retailStoreIdNotEmptyQueryPart]) 
    , MethodTryToGet 
        (MethodInfo "TryToGetPricesRequest" (ResponseTArray "PriceResponse") [retailStoreIdNotEmptyField, itemIdNotEmptyField, eanField])
        (UrlBuilder [UrlPartLit "products", UrlPartVar itemIdNotEmptyField, UrlPartLit "prices"] [retailStoreIdNotEmptyQueryPart, eanQueryPart]) 
    , MethodTryToGet 
        (MethodInfo "TryToGetOtherStoresStockRequest" (ResponseTArray "OtherStoresStockServerResponse") [itemIdNotEmptyField, chainField])
        (UrlBuilder [UrlPartLit "products", UrlPartVar itemIdNotEmptyField, UrlPartLit "stock/chain", UrlPartVar chainField] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetWithdrawalExternalInfoRequest" (ResponseT "WithdrawalExternalInfo") [itemIdField, expirationDateField, retailStoreIdNotEmptyField])
        (UrlBuilder [UrlPartLit "products", UrlPartVar itemIdField, UrlPartLit "expirations/quantities"] [retailStoreIdNotEmptyQueryPart, expirationDateQueryPart])
    , MethodTryToPost
        (MethodInfo "TryToGetSendFutureValiditiesRequest" (ResponseT "Response") [itemIdNotEmptyField, CustomField "SendFutureValiditiesRequest" "sendFutureValiditiesRequest"])
        "SendFutureValiditiesRequestToSend"
        (UrlBuilder [UrlPartLit "products", UrlPartVar itemIdNotEmptyField, UrlPartLit "validities"] [])
    ]

createItemListsServerRequestsFile = TemplateSimpleGet 
    []
    "ItemLists" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetItemListsRequest" (ResponseTArray "ItemListEntryResponse") []) 
        (UrlBuilder [UrlPartLit "item-lists"] [])
    ]

createDocumentsServerRequestsFile = TemplateSimpleGet 
    []
    "Documents" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetGetAvailableLabelsRequest" (ResponseTArray "DocumentEntryResponse") [retailStoreIdField])
        (UrlBuilder [UrlPartLit "documents"] [retailStoreIdQueryPart]) 
    ]

createPrintersServerRequestsFile = 
    let 
        macAddressField = StringNotEmptyField "macAddress"
    in TemplateSimpleGet 
    ["Tlantic.Functional"]
    "Printers" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetPrinterRequest" (ResponseT "PrinterEntryReponse") [macAddressField])
        (UrlBuilder [UrlPartLit "printers", UrlPartVar macAddressField] [])
    , MethodTryToGet 
        (MethodInfo "TryToGetPrintersRequest" (ResponseTArray "PrinterEntryReponse") [retailStoreIdField])
        (UrlBuilder [UrlPartLit "printers"] [retailStoreIdQueryPart])
    ]

createChecklistsServerRequestsFile = TemplateSimpleGet 
    []
    "Checklists" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetChecklistsRequest" (ResponseTArray "ChecklistEntryResponse") [])
        (UrlBuilder [UrlPartLit "checklists"] [])
    ]
    
createBarcodesServerRequestsFile = TemplateSimpleGet 
    []
    "Barcodes" 
    [ MethodTryToGet 
        (MethodInfo "TryToGetScanCodeRulesRequest" (ResponseTArray "BarcodeRule") [])
        (UrlBuilder [UrlPartLit "barcodes/rules"] [])
    ]
    