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
        public IChoiceGetRequestWithRetry<Response<FutureDatesDto>,NetworkError> TryToGetFutureDatesRequest(string itemId,StringNotEmpty retailStoreId)
        {
            var parts = new UrlParts("resources/items/sku",itemId,"expirations");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId.Value));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<FutureDatesDto>(urlBuilder);
        }
    }
}