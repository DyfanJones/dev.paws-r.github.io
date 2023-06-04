<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>glue_get_crawlers</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves metadata for all crawlers defined in the customer account

### Description

Retrieves metadata for all crawlers defined in the customer account.

### Usage

    glue_get_crawlers(MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="glue_get_crawlers_:_MaxResults">MaxResults</code></td>
<td><p>The number of crawlers to return on each call.</p></td>
</tr>
<tr class="even">
<td><code id="glue_get_crawlers_:_NextToken">NextToken</code></td>
<td><p>A continuation token, if this is a continuation request.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Crawlers = list(
        list(
          Name = "string",
          Role = "string",
          Targets = list(
            S3Targets = list(
              list(
                Path = "string",
                Exclusions = list(
                  "string"
                ),
                ConnectionName = "string",
                SampleSize = 123,
                EventQueueArn = "string",
                DlqEventQueueArn = "string"
              )
            ),
            JdbcTargets = list(
              list(
                ConnectionName = "string",
                Path = "string",
                Exclusions = list(
                  "string"
                ),
                EnableAdditionalMetadata = list(
                  "COMMENTS"|"RAWTYPES"
                )
              )
            ),
            MongoDBTargets = list(
              list(
                ConnectionName = "string",
                Path = "string",
                ScanAll = TRUE|FALSE
              )
            ),
            DynamoDBTargets = list(
              list(
                Path = "string",
                scanAll = TRUE|FALSE,
                scanRate = 123.0
              )
            ),
            CatalogTargets = list(
              list(
                DatabaseName = "string",
                Tables = list(
                  "string"
                ),
                ConnectionName = "string",
                EventQueueArn = "string",
                DlqEventQueueArn = "string"
              )
            ),
            DeltaTargets = list(
              list(
                DeltaTables = list(
                  "string"
                ),
                ConnectionName = "string",
                WriteManifest = TRUE|FALSE,
                CreateNativeDeltaTable = TRUE|FALSE
              )
            )
          ),
          DatabaseName = "string",
          Description = "string",
          Classifiers = list(
            "string"
          ),
          RecrawlPolicy = list(
            RecrawlBehavior = "CRAWL_EVERYTHING"|"CRAWL_NEW_FOLDERS_ONLY"|"CRAWL_EVENT_MODE"
          ),
          SchemaChangePolicy = list(
            UpdateBehavior = "LOG"|"UPDATE_IN_DATABASE",
            DeleteBehavior = "LOG"|"DELETE_FROM_DATABASE"|"DEPRECATE_IN_DATABASE"
          ),
          LineageConfiguration = list(
            CrawlerLineageSettings = "ENABLE"|"DISABLE"
          ),
          State = "READY"|"RUNNING"|"STOPPING",
          TablePrefix = "string",
          Schedule = list(
            ScheduleExpression = "string",
            State = "SCHEDULED"|"NOT_SCHEDULED"|"TRANSITIONING"
          ),
          CrawlElapsedTime = 123,
          CreationTime = as.POSIXct(
            "2015-01-01"
          ),
          LastUpdated = as.POSIXct(
            "2015-01-01"
          ),
          LastCrawl = list(
            Status = "SUCCEEDED"|"CANCELLED"|"FAILED",
            ErrorMessage = "string",
            LogGroup = "string",
            LogStream = "string",
            MessagePrefix = "string",
            StartTime = as.POSIXct(
              "2015-01-01"
            )
          ),
          Version = 123,
          Configuration = "string",
          CrawlerSecurityConfiguration = "string",
          LakeFormationConfiguration = list(
            UseLakeFormationCredentials = TRUE|FALSE,
            AccountId = "string"
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$get_crawlers(
      MaxResults = 123,
      NextToken = "string"
    )