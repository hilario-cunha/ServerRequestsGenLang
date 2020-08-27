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
            var urlBuilder = CreateUrlBuilderTryToGetHierarchicalStructureRequest(rootHsId);
            return serverConfig.TryToGet<HierarchicalStructureEntry[]>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetHierarchicalStructureRequest(string rootHsId)
        {
            var parts = new UrlParts("hierarchicalstructure",rootHsId);
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
    }
}