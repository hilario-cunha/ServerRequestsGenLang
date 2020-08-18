using Tlantic.Server.Core;
using System;
using Tlantic.Functional;
using System.Collections.Generic;
using Tlantic.Server.Internal.Dtos;
namespace Tlantic.Server.TaskIntegrations
{
    public partial class TaskIntegrationsServerRequests
    {
        ServerConfig serverConfig;
        internal TaskIntegrationsServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetCreateNewFutureDateByIntegrationRequest(StringNotEmpty retailStoreId,List<NewFutureDateByIntegrationRequestResource> resources)
        {
            var parts = new UrlParts("task-integrations");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            var data = TryToGetCreateNewFutureDateByIntegrationRequestMapData(retailStoreId,resources);
            return serverConfig.TryToPost<NewFutureDateByIntegrationRequest,Response>(urlBuilder,data);
        }
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetCreateTaskAsyncRequest(CreateTaskAsyncRequest createTaskAsyncRequest)
        {
            var parts = new UrlParts("task-integrations");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            var data = TryToGetCreateTaskAsyncRequestMapData(createTaskAsyncRequest);
            return serverConfig.TryToPost<CreateTaskAsyncRequestToSend,Response>(urlBuilder,data);
        }
    }
}