u [Tlantic.Functional]
f Products
m TryToGetExpirationsItemParametersRequest ExpirationsItemParameters [String itemId, StringNotEmpty retailStoreId]
products/{itemId}/expirations/parameters?store={retailStoreId}
m TryToGetDailySalesServerRequest Sales [StringNotEmpty retailStoreId, String ean, DateTime date]
products/eans/{ean}/sales/{retailStoreId}/{date}
m TryToGetDamagesDestinationsRequest [DamagesDestinationsResponse] [StringNotEmpty retailStoreId]
products/damages/{retailStoreId}/destinations
m TryToGetDamagesReasonsRequest [DamagesReasonsResponse] []
products/damages/reasons
m TryToGetExternalPVPRequest PriceValue [String itemId, StringNotEmpty retailStoreId]
products/{itemId}/price?store={retailStoreId}
m TryToGetPricesRequest [PriceResponse] [StringNotEmpty retailStoreId, StringNotEmpty itemId, String ean]
products/{itemId}/prices?store={retailStoreId}&ean={ean}
m TryToGetOtherStoresStockRequest [OtherStoresStockServerResponse] [StringNotEmpty itemId, StringNotEmpty chainValue]
products/{itemId}/stock/chain/{chainValue}
m TryToGetWithdrawalExternalInfoRequest WithdrawalExternalInfo [String itemId, DateTime expirationDate, StringNotEmpty retailStoreId]
products/{itemId}/expirations/quantities?store={retailStoreId}&date={expirationDate}
p TryToGetSendFutureValiditiesRequest () [StringNotEmpty itemId] SendFutureValiditiesRequestToSend
products/{itemId}/validities
