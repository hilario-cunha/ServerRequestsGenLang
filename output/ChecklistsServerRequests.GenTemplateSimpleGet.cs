using Tlantic.Server.Core;
using System;
namespace Tlantic.Server.Checklists
{
    public partial class ChecklistsServerRequests
    {
        ServerConfig serverConfig;
        internal ChecklistsServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<ChecklistEntryResponse[]>,NetworkError> TryToGetChecklistsRequest()
        {
            var urlBuilder = CreateUrlBuilderTryToGetChecklistsRequest();
            return serverConfig.TryToGet<Response<ChecklistEntryResponse[]>>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetChecklistsRequest()
        {
            var parts = new UrlParts("checklists");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
    }
}