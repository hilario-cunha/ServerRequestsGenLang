using Tlantic.Server.Core;
using System;
using Tlantic.Functional;
namespace Tlantic.Server.Printers
{
    public partial class PrintersServerRequests
    {
        ServerConfig serverConfig;
        internal PrintersServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<PrinterEntryReponse>,NetworkError> TryToGetPrinterRequest(StringNotEmpty macAddress)
        {
            var urlBuilder = CreateUrlBuilderTryToGetPrinterRequest(macAddress);
            return serverConfig.TryToGet<Response<PrinterEntryReponse>>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<PrinterEntryReponse[]>,NetworkError> TryToGetPrintersRequest(string retailStoreId)
        {
            var urlBuilder = CreateUrlBuilderTryToGetPrintersRequest(retailStoreId);
            return serverConfig.TryToGet<Response<PrinterEntryReponse[]>>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetPrinterRequest(StringNotEmpty macAddress)
        {
            var parts = new UrlParts("printers",macAddress.Value);
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public UrlBuilder CreateUrlBuilderTryToGetPrintersRequest(string retailStoreId)
        {
            var parts = new UrlParts("printers");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId));
            return new UrlBuilder(parts,queryParts);
        }
    }
}