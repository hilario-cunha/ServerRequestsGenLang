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
        public IChoiceGetRequestWithRetry<Response<UserInfo>,NetworkError> TryToGetUserInfo()
        {
            var parts = new UrlParts("users/user");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<UserInfo>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<List<Store>>,NetworkError> TryToGetUserStoresRequest()
        {
            var parts = new UrlParts("user/stores");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<List<Store>>(urlBuilder);
        }
    }
}