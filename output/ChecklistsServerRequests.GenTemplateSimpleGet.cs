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
        public UrlBuilder CreateUrlBuilderTryToGetChecklistsRequest()
        {
            var parts = new UrlParts("checklists");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<ChecklistEntryResponse[]>,NetworkError> TryToGetChecklistsRequest()
        {
            var urlBuilder = CreateUrlBuilderTryToGetChecklistsRequest();
            return serverConfig.TryToGet<ChecklistEntryResponse[]>(urlBuilder);
        }
    }
}