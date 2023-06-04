<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>athena_get_query_execution</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns information about a single execution of a query if you have access to the workgroup in which the query ran

### Description

Returns information about a single execution of a query if you have
access to the workgroup in which the query ran. Each time a query
executes, information about the query execution is saved with a unique
ID.

### Usage

    athena_get_query_execution(QueryExecutionId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="athena_get_query_execution_:_QueryExecutionId">QueryExecutionId</code></td>
<td><p>[required] The unique ID of the query execution.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      QueryExecution = list(
        QueryExecutionId = "string",
        Query = "string",
        StatementType = "DDL"|"DML"|"UTILITY",
        ResultConfiguration = list(
          OutputLocation = "string",
          EncryptionConfiguration = list(
            EncryptionOption = "SSE_S3"|"SSE_KMS"|"CSE_KMS",
            KmsKey = "string"
          ),
          ExpectedBucketOwner = "string",
          AclConfiguration = list(
            S3AclOption = "BUCKET_OWNER_FULL_CONTROL"
          )
        ),
        ResultReuseConfiguration = list(
          ResultReuseByAgeConfiguration = list(
            Enabled = TRUE|FALSE,
            MaxAgeInMinutes = 123
          )
        ),
        QueryExecutionContext = list(
          Database = "string",
          Catalog = "string"
        ),
        Status = list(
          State = "QUEUED"|"RUNNING"|"SUCCEEDED"|"FAILED"|"CANCELLED",
          StateChangeReason = "string",
          SubmissionDateTime = as.POSIXct(
            "2015-01-01"
          ),
          CompletionDateTime = as.POSIXct(
            "2015-01-01"
          ),
          AthenaError = list(
            ErrorCategory = 123,
            ErrorType = 123,
            Retryable = TRUE|FALSE,
            ErrorMessage = "string"
          )
        ),
        Statistics = list(
          EngineExecutionTimeInMillis = 123,
          DataScannedInBytes = 123,
          DataManifestLocation = "string",
          TotalExecutionTimeInMillis = 123,
          QueryQueueTimeInMillis = 123,
          QueryPlanningTimeInMillis = 123,
          ServiceProcessingTimeInMillis = 123,
          ResultReuseInformation = list(
            ReusedPreviousResult = TRUE|FALSE
          )
        ),
        WorkGroup = "string",
        EngineVersion = list(
          SelectedEngineVersion = "string",
          EffectiveEngineVersion = "string"
        ),
        ExecutionParameters = list(
          "string"
        ),
        SubstatementType = "string"
      )
    )

### Request syntax

    svc$get_query_execution(
      QueryExecutionId = "string"
    )