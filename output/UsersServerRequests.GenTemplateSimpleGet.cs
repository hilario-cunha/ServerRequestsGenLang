using Tlantic.Server.Core;
using System;
using System.Collections.Generic;
namespace Tlantic.Server.Users
{
    public partial class UsersServerRequests
    {
        ServerConfig serverConfig;
        internal UsersServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public UrlBuilder CreateUrlBuilderTryToGetUserInfo()
        {
            var parts = new UrlParts("users","user");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<UserInfo>,NetworkError> TryToGetUserInfo()
        {
            var urlBuilder = CreateUrlBuilderTryToGetUserInfo();
            return serverConfig.TryToGet<UserInfo>(urlBuilder);
        }
        public UrlBuilder CreateUrlBuilderTryToGetUserStoresRequest()
        {
            var parts = new UrlParts("user","stores");
            var queryParts = new UrlQueryParameters();
            return new UrlBuilder(parts,queryParts);
        }
        public IChoiceGetRequestWithRetry<Response<List<Store>>,NetworkError> TryToGetUserStoresRequest()
        {
            var urlBuilder = CreateUrlBuilderTryToGetUserStoresRequest();
            return serverConfig.TryToGet<List<Store>>(urlBuilder);
        }
    }
}