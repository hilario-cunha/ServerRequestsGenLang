using Tlantic.Server.Core;
using System;
using Tlantic.Functional;
namespace Tlantic.Server.Resources
{
    public partial class ResourcesServerRequests
    {
        ServerConfig serverConfig;
        internal ResourcesServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public UrlBuilder CreateUrlBuilderTryToGetFutureDatesRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("resources","items","sku",itemId,"expirations");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value));
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<FutureDatesDto>,NetworkError> TryToGetFutureDatesRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var urlBuilder = CreateUrlBuilderTryToGetFutureDatesRequest(itemId,retailStoreId);
            return serverConfig.TryToGet<FutureDatesDto>(urlBuilder);
        }
    }
}