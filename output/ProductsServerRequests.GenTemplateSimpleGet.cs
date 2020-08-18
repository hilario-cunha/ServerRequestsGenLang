using Tlantic.Server.Core;
using System;
using Tlantic.Functional;
namespace Tlantic.Server.Products
{
    public partial class ProductsServerRequests
    {
        ServerConfig serverConfig;
        internal ProductsServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<ExpirationsItemParameters>,NetworkError> TryToGetExpirationsItemParametersRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("products",itemId,"expirations/parameters");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<ExpirationsItemParameters>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<Sales>,NetworkError> TryToGetDailySalesServerRequest(StringNotEmpty retailStoreId,string ean,DateTime date)
        {
            var parts = new UrlParts("products/eans",ean,"sales",retailStoreId.Value,HttpUtils.DateTimeZoneHandlingUtcIso8601(date));
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<Sales>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<DamagesDestinationsResponse[]>,NetworkError> TryToGetDamagesDestinationsRequest(StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("products/damages",retailStoreId.Value,"destinations");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<DamagesDestinationsResponse[]>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<DamagesReasonsResponse[]>,NetworkError> TryToGetDamagesReasonsRequest()
        {
            var parts = new UrlParts("products/damages/reasons");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<DamagesReasonsResponse[]>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<PriceValue>,NetworkError> TryToGetExternalPVPRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("products",itemId,"price");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<PriceValue>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<PriceResponse[]>,NetworkError> TryToGetPricesRequest(StringNotEmpty retailStoreId,StringNotEmpty itemId,string ean)
        {
            var parts = new UrlParts("products",itemId.Value,"prices");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value),new UrlQueryParameter("ean",ean));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<PriceResponse[]>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<OtherStoresStockServerResponse[]>,NetworkError> TryToGetOtherStoresStockRequest(StringNotEmpty itemId,StringNotEmpty chain)
        {
            var parts = new UrlParts("products",itemId.Value,"stock/chain",chain.Value);
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<OtherStoresStockServerResponse[]>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<WithdrawalExternalInfo>,NetworkError> TryToGetWithdrawalExternalInfoRequest(string itemId,DateTime expirationDate,StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("products",itemId,"expirations/quantities");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value),new UrlQueryParameter("date",HttpUtils.DateTimeZoneHandlingUtcIso8601(expirationDate)));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<WithdrawalExternalInfo>(urlBuilder);
        }
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetSendFutureValiditiesRequest(StringNotEmpty itemId,SendFutureValiditiesRequest sendFutureValiditiesRequest)
        {
            var parts = new UrlParts("products",itemId.Value,"validities");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            var data = TryToGetSendFutureValiditiesRequestMapData(itemId,sendFutureValiditiesRequest);
            return serverConfig.TryToPost<SendFutureValiditiesRequestToSend,Response>(urlBuilder,data);
        }
    }
}