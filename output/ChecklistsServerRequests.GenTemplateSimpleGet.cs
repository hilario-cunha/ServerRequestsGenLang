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
            var parts = new UrlParts("checklists");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<ChecklistEntryResponse[]>(urlBuilder);
        }
    }
}