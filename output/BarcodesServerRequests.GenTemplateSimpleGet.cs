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
            var urlBuilder = CreateUrlBuilderTryToGetScanCodeRulesRequest();
            return serverConfig.TryToGet<Response<BarcodeRule[]>>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetScanCodeRulesRequest()
        {
            var parts = new UrlParts("barcodes","rules");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
    }
}