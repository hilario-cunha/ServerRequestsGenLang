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
        public UrlBuilder CreateUrlBuilderTryToGetCreateNewFutureDateByIntegrationRequest()
        {
            var parts = new UrlParts("task-integrations");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetCreateNewFutureDateByIntegrationRequest(NewFutureDateByIntegrationRequest data)
        {
            var urlBuilder = CreateUrlBuilderTryToGetCreateNewFutureDateByIntegrationRequest();
            return serverConfig.TryToPost<NewFutureDateByIntegrationRequest,Response>(urlBuilder,data);
        }
        public UrlBuilder CreateUrlBuilderTryToGetCreateTaskAsyncRequest()
        {
            var parts = new UrlParts("task-integrations");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoicePostRequestWithRetry<Response,NetworkError> TryToGetCreateTaskAsyncRequest(CreateTaskAsyncRequestToSend data)
        {
            var urlBuilder = CreateUrlBuilderTryToGetCreateTaskAsyncRequest();
            return serverConfig.TryToPost<CreateTaskAsyncRequestToSend,Response>(urlBuilder,data);
        }
    }
}