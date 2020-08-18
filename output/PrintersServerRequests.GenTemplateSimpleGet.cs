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
            var parts = new UrlParts("printers",macAddress.Value);
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<PrinterEntryReponse>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<PrinterEntryReponse[]>,NetworkError> TryToGetPrintersRequest(string retailStoreId)
        {
            var parts = new UrlParts("printers");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<PrinterEntryReponse[]>(urlBuilder);
        }
    }
}