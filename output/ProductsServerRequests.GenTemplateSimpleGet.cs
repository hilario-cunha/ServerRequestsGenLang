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
        public UrlBuilder CreateUrlBuilderTryToGetExpirationsItemParametersRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("products",itemId,"expirations","parameters");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value));
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<ExpirationsItemParameters>,NetworkError> TryToGetExpirationsItemParametersRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var urlBuilder = CreateUrlBuilderTryToGetExpirationsItemParametersRequest(itemId,retailStoreId);
            return serverConfig.TryToGet<ExpirationsItemParameters>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetDailySalesServerRequest(string ean,StringNotEmpty retailStoreId,DateTime date)
        {
            var parts = new UrlParts("products","eans",ean,"sales",retailStoreId.Value,HttpUtils.DateTimeZoneHandlingUtcIso8601(date));
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<Sales>,NetworkError> TryToGetDailySalesServerRequest(StringNotEmpty retailStoreId,string ean,DateTime date)
        {
            var urlBuilder = CreateUrlBuilderTryToGetDailySalesServerRequest(ean,retailStoreId,date);
            return serverConfig.TryToGet<Sales>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetDamagesDestinationsRequest(StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("products","damages",retailStoreId.Value,"destinations");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<DamagesDestinationsResponse[]>,NetworkError> TryToGetDamagesDestinationsRequest(StringNotEmpty retailStoreId)
        {
            var urlBuilder = CreateUrlBuilderTryToGetDamagesDestinationsRequest(retailStoreId);
            return serverConfig.TryToGet<DamagesDestinationsResponse[]>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetDamagesReasonsRequest()
        {
            var parts = new UrlParts("products","damages","reasons");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<DamagesReasonsResponse[]>,NetworkError> TryToGetDamagesReasonsRequest()
        {
            var urlBuilder = CreateUrlBuilderTryToGetDamagesReasonsRequest();
            return serverConfig.TryToGet<DamagesReasonsResponse[]>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetExternalPVPRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("products",itemId,"price");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value));
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<PriceValue>,NetworkError> TryToGetExternalPVPRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var urlBuilder = CreateUrlBuilderTryToGetExternalPVPRequest(itemId,retailStoreId);
            return serverConfig.TryToGet<PriceValue>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetPricesRequest(StringNotEmpty itemId,StringNotEmpty retailStoreId,string ean)
        {
            var parts = new UrlParts("products",itemId.Value,"prices");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value),new UrlQueryParameter("ean",ean));
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<PriceResponse[]>,NetworkError> TryToGetPricesRequest(StringNotEmpty retailStoreId,StringNotEmpty itemId,string ean)
        {
            var urlBuilder = CreateUrlBuilderTryToGetPricesRequest(itemId,retailStoreId,ean);
            return serverConfig.TryToGet<PriceResponse[]>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetOtherStoresStockRequest(StringNotEmpty itemId,StringNotEmpty chainValue)
        {
            var parts = new UrlParts("products",itemId.Value,"stock","chain",chainValue.Value);
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<OtherStoresStockServerResponse[]>,NetworkError> TryToGetOtherStoresStockRequest(StringNotEmpty itemId,StringNotEmpty chainValue)
        {
            var urlBuilder = CreateUrlBuilderTryToGetOtherStoresStockRequest(itemId,chainValue);
            return serverConfig.TryToGet<OtherStoresStockServerResponse[]>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetWithdrawalExternalInfoRequest(string itemId,StringNotEmpty retailStoreId,DateTime expirationDate)
        {
            var parts = new UrlParts("products",itemId,"expirations","quantities");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value),new UrlQueryParameter("date",HttpUtils.DateTimeZoneHandlingUtcIso8601(expirationDate)));
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<WithdrawalExternalInfo>,NetworkError> TryToGetWithdrawalExternalInfoRequest(string itemId,DateTime expirationDate,StringNotEmpty retailStoreId)
        {
            var urlBuilder = CreateUrlBuilderTryToGetWithdrawalExternalInfoRequest(itemId,retailStoreId,expirationDate);
            return serverConfig.TryToGet<WithdrawalExternalInfo>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetSendFutureValiditiesRequest(StringNotEmpty itemId)
        {
            var parts = new UrlParts("products",itemId.Value,"validities");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetSendFutureValiditiesRequest(StringNotEmpty itemId,SendFutureValiditiesRequestToSend data)
        {
            var urlBuilder = CreateUrlBuilderTryToGetSendFutureValiditiesRequest(itemId);
            return serverConfig.TryToPost<SendFutureValiditiesRequestToSend,Response>(urlBuilder,data);
        }
    }
}