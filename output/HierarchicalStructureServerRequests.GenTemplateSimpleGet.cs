using Tlantic.Server.Core;
using System;
namespace Tlantic.Server.HierarchicalStructure
{
    public partial class HierarchicalStructureServerRequests
    {
        ServerConfig serverConfig;
        internal HierarchicalStructureServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<HierarchicalStructureEntry[]>,NetworkError> TryToGetHierarchicalStructureRequest(string rootHsId)
        {
            var parts = new UrlParts("hierarchicalstructure",rootHsId);
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<HierarchicalStructureEntry[]>(urlBuilder);
        }
    }
}