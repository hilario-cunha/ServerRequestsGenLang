using Tlantic.Server.Core;
using System;
namespace Tlantic.Server.Expirations
{
    public partial class ExpirationsServerRequests
    {
        ServerConfig serverConfig;
        internal ExpirationsServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public UrlBuilder CreateUrlBuilderTryToGetDeleteFutureDatesRequest(string itemId)
        {
            var parts = new UrlParts("expirations",itemId + ":deletebatch");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetDeleteFutureDatesRequest(string itemId,FutureDatesToDeleteDto data)
        {
            var urlBuilder = CreateUrlBuilderTryToGetDeleteFutureDatesRequest(itemId);
            return serverConfig.TryToPost<FutureDatesToDeleteDto,Response>(urlBuilder,data);
        }
    }
}