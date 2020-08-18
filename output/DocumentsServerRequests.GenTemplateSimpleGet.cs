using Tlantic.Server.Core;
using System;
namespace Tlantic.Server.Documents
{
    public partial class DocumentsServerRequests
    {
        ServerConfig serverConfig;
        internal DocumentsServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<DocumentEntryResponse[]>,NetworkError> TryToGetGetAvailableLabelsRequest(string retailStoreId)
        {
            var parts = new UrlParts("documents");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<DocumentEntryResponse[]>(urlBuilder);
        }
    }
}