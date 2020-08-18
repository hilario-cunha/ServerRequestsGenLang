using Tlantic.Server.Core;
using System;
namespace Tlantic.Server.ItemLists
{
    public partial class ItemListsServerRequests
    {
        ServerConfig serverConfig;
        internal ItemListsServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<ItemListEntryResponse[]>,NetworkError> TryToGetItemListsRequest()
        {
            var parts = new UrlParts("item-lists");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<ItemListEntryResponse[]>(urlBuilder);
        }
    }
}