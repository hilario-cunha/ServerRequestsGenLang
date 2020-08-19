using Tlantic.Server.Core;
using System;
namespace Tlantic.Server.Barcodes
{
    public partial class BarcodesServerRequests
    {
        ServerConfig serverConfig;
        internal BarcodesServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<BarcodeRule[]>,NetworkError> TryToGetScanCodeRulesRequest()
        {
            var parts = new UrlParts("barcodes","rules");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<BarcodeRule[]>(urlBuilder);
        }
    }
}