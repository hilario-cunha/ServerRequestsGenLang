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
        public UrlBuilder CreateUrlBuilderTryToGetItemListsRequest()
        {
            var parts = new UrlParts("item-lists");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<ItemListEntryResponse[]>,NetworkError> TryToGetItemListsRequest()
        {
            var urlBuilder = CreateUrlBuilderTryToGetItemListsRequest();
            return serverConfig.TryToGet<ItemListEntryResponse[]>(urlBuilder);
        }
    }
}