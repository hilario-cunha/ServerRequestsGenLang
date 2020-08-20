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
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetDeleteFutureDatesRequest(string itemId,FutureDatesToDeleteDto data)
        {
            var parts = new UrlParts("expirations",itemId + ":deletebatch");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToPost<FutureDatesToDeleteDto,Response>(urlBuilder,data);
        }
    }
}