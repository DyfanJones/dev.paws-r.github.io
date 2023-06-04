<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>apigateway_get_stage</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets information about a Stage resource

### Description

Gets information about a Stage resource.

### Usage

    apigateway_get_stage(restApiId, stageName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="apigateway_get_stage_:_restApiId">restApiId</code></td>
<td><p>[required] The string identifier of the associated
RestApi.</p></td>
</tr>
<tr class="even">
<td><code id="apigateway_get_stage_:_stageName">stageName</code></td>
<td><p>[required] The name of the Stage resource to get information
about.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      deploymentId = "string",
      clientCertificateId = "string",
      stageName = "string",
      description = "string",
      cacheClusterEnabled = TRUE|FALSE,
      cacheClusterSize = "0.5"|"1.6"|"6.1"|"13.5"|"28.4"|"58.2"|"118"|"237",
      cacheClusterStatus = "CREATE_IN_PROGRESS"|"AVAILABLE"|"DELETE_IN_PROGRESS"|"NOT_AVAILABLE"|"FLUSH_IN_PROGRESS",
      methodSettings = list(
        list(
          metricsEnabled = TRUE|FALSE,
          loggingLevel = "string",
          dataTraceEnabled = TRUE|FALSE,
          throttlingBurstLimit = 123,
          throttlingRateLimit = 123.0,
          cachingEnabled = TRUE|FALSE,
          cacheTtlInSeconds = 123,
          cacheDataEncrypted = TRUE|FALSE,
          requireAuthorizationForCacheControl = TRUE|FALSE,
          unauthorizedCacheControlHeaderStrategy = "FAIL_WITH_403"|"SUCCEED_WITH_RESPONSE_HEADER"|"SUCCEED_WITHOUT_RESPONSE_HEADER"
        )
      ),
      variables = list(
        "string"
      ),
      documentationVersion = "string",
      accessLogSettings = list(
        format = "string",
        destinationArn = "string"
      ),
      canarySettings = list(
        percentTraffic = 123.0,
        deploymentId = "string",
        stageVariableOverrides = list(
          "string"
        ),
        useStageCache = TRUE|FALSE
      ),
      tracingEnabled = TRUE|FALSE,
      webAclArn = "string",
      tags = list(
        "string"
      ),
      createdDate = as.POSIXct(
        "2015-01-01"
      ),
      lastUpdatedDate = as.POSIXct(
        "2015-01-01"
      )
    )

### Request syntax

    svc$get_stage(
      restApiId = "string",
      stageName = "string"
    )