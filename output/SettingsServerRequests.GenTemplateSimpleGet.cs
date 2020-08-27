using Tlantic.Server.Core;
using System;
using System.Collections.Generic;
using Tlantic.Server.Internal.Dtos;
namespace Tlantic.Server.Settings
{
    public partial class SettingsServerRequests
    {
        ServerConfig serverConfig;
        internal SettingsServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public UrlBuilder CreateUrlBuilderTryToGetGetSettingsForCodesRequestServer()
        {
            var parts = new UrlParts("settings","entity","application");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoicePostRequestWithRetry<Response<List<Setting>>,NetworkError> TryToGetGetSettingsForCodesRequestServer(SettingsRequest data)
        {
            var urlBuilder = CreateUrlBuilderTryToGetGetSettingsForCodesRequestServer();
            return serverConfig.TryToPost<SettingsRequest,Response<List<Setting>>>(urlBuilder,data);
        }
    }
}