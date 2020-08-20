using Tlantic.Server.Core;
using System;
using Tlantic.Functional;
using Tlantic.Server.Internal.Dtos;
namespace Tlantic.Server.Tasks
{
    public partial class TasksServerRequests
    {
        ServerConfig serverConfig;
        internal TasksServerRequests(ServerConfig serverConfig)
        {
            this.serverConfig = serverConfig;
        }
        public IChoiceGetRequestWithRetry<Response<TasksInstoreAdapterResponse[]>,NetworkError> TryToGetTasksInstoreAdapterRequest(int offset,int limit,string retailStoreId,StringNotEmpty[] status,StringNotEmpty[] typesOfTasks,string search,string parentTaskId,DateTime? fromScheduledStart,DateTime? toScheduledStart)
        {
            var parts = new UrlParts("tasks");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("adapter","instore-adapter"),new UrlQueryParameter("offset",offset.ToString()),new UrlQueryParameter("limit",limit.ToString()),new UrlQueryParameter("store",retailStoreId),new UrlQueryParameter("status",status),new UrlQueryParameter("type",typesOfTasks),new UrlQueryParameter("search",search),new UrlQueryParameter("parent",parentTaskId),new UrlQueryParameter("from_scheduled_start",HttpUtils.DateTimeZoneHandlingUtcIso8601(fromScheduledStart.Value)),new UrlQueryParameter("to_scheduled_start",HttpUtils.DateTimeZoneHandlingUtcIso8601(toScheduledStart.Value)));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<TasksInstoreAdapterResponse[]>(urlBuilder);
        }
        public IChoiceGetRequestWithRetry<Response<TasksSummaryResponse>,NetworkError> TryToGetTasksSummaryRequest(string retailStoreId,StringNotEmpty[] status,DateTime? fromScheduledStart,DateTime? toScheduledStart)
        {
            var parts = new UrlParts("tasks-summary");
            var queryParts = new UrlQueryParameters(new UrlQueryParameter("store",retailStoreId),new UrlQueryParameter("status",status),new UrlQueryParameter("from_scheduled_start",HttpUtils.DateTimeZoneHandlingUtcIso8601(fromScheduledStart.Value)),new UrlQueryParameter("to_scheduled_start",HttpUtils.DateTimeZoneHandlingUtcIso8601(toScheduledStart.Value)));
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToGet<TasksSummaryResponse>(urlBuilder);
        }
        public IChoicePostRequestWithRetry<Response<CreateTaskResponse>,NetworkError> TryToGetCreateTaskRequest(CreateTaskRequest createTaskRequest,CreateTaskRequestToSend data)
        {
            var parts = new UrlParts("tasks");
            var queryParts = new UrlQueryParameters();
            var urlBuilder = new UrlBuilder(parts,queryParts);
            return serverConfig.TryToPost<CreateTaskRequestToSend,Response<CreateTaskResponse>>(urlBuilder,data);
        }
    }
}