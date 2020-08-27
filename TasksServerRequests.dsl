u [Tlantic.Functional, Tlantic.Server.Internal.Dtos]
f Tasks
m TryToGetTasksInstoreAdapterRequest [TasksInstoreAdapterResponse] [Int offset, Int limit, String retailStoreId, StringNotEmptyArray status, StringNotEmptyArray typesOfTasks, String search, String parentTaskId, DateTimeNullable fromScheduledStart, DateTimeNullable toScheduledStart]
tasks?adapter=instore-adapter&offset={offset}&limit={limit}&store={retailStoreId}&status={status}&type={typesOfTasks}&search={search}&parent={parentTaskId}&from_scheduled_start={fromScheduledStart}&to_scheduled_start={toScheduledStart}
m TryToGetTasksSummaryRequest TasksSummaryResponse [String retailStoreId, StringNotEmptyArray status, DateTimeNullable fromScheduledStart, DateTimeNullable toScheduledStart]
tasks-summary?store={retailStoreId}&status={status}&from_scheduled_start={fromScheduledStart}&to_scheduled_start={toScheduledStart}
p TryToGetCreateTaskRequest CreateTaskResponse [] CreateTaskRequestToSend
tasks
