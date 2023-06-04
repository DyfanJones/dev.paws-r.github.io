<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>servicequotas_list_service_quotas</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists the applied quota values for the specified AWS service

### Description

Lists the applied quota values for the specified AWS service. For some
quotas, only the default values are available. If the applied quota
value is not available for a quota, the quota is not retrieved.

### Usage

    servicequotas_list_service_quotas(ServiceCode, NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="servicequotas_list_service_quotas_:_ServiceCode">ServiceCode</code></td>
<td><p>[required] The service identifier.</p></td>
</tr>
<tr class="even">
<td><code
id="servicequotas_list_service_quotas_:_NextToken">NextToken</code></td>
<td><p>The token for the next page of results.</p></td>
</tr>
<tr class="odd">
<td><code
id="servicequotas_list_service_quotas_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of results to return with a single call. To
retrieve the remaining results, if any, make another call with the token
returned from this call.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      NextToken = "string",
      Quotas = list(
        list(
          ServiceCode = "string",
          ServiceName = "string",
          QuotaArn = "string",
          QuotaCode = "string",
          QuotaName = "string",
          Value = 123.0,
          Unit = "string",
          Adjustable = TRUE|FALSE,
          GlobalQuota = TRUE|FALSE,
          UsageMetric = list(
            MetricNamespace = "string",
            MetricName = "string",
            MetricDimensions = list(
              "string"
            ),
            MetricStatisticRecommendation = "string"
          ),
          Period = list(
            PeriodValue = 123,
            PeriodUnit = "MICROSECOND"|"MILLISECOND"|"SECOND"|"MINUTE"|"HOUR"|"DAY"|"WEEK"
          ),
          ErrorReason = list(
            ErrorCode = "DEPENDENCY_ACCESS_DENIED_ERROR"|"DEPENDENCY_THROTTLING_ERROR"|"DEPENDENCY_SERVICE_ERROR"|"SERVICE_QUOTA_NOT_AVAILABLE_ERROR",
            ErrorMessage = "string"
          )
        )
      )
    )

### Request syntax

    svc$list_service_quotas(
      ServiceCode = "string",
      NextToken = "string",
      MaxResults = 123
    )