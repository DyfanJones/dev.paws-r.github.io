<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>servicequotas_list_requested_service_quota_change_history</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves the quota increase requests for the specified service

### Description

Retrieves the quota increase requests for the specified service.

### Usage

    servicequotas_list_requested_service_quota_change_history(ServiceCode,
      Status, NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="servicequotas_list_requested_service_quota_change_history_:_ServiceCode">ServiceCode</code></td>
<td><p>The service identifier.</p></td>
</tr>
<tr class="even">
<td><code
id="servicequotas_list_requested_service_quota_change_history_:_Status">Status</code></td>
<td><p>The status of the quota increase request.</p></td>
</tr>
<tr class="odd">
<td><code
id="servicequotas_list_requested_service_quota_change_history_:_NextToken">NextToken</code></td>
<td><p>The token for the next page of results.</p></td>
</tr>
<tr class="even">
<td><code
id="servicequotas_list_requested_service_quota_change_history_:_MaxResults">MaxResults</code></td>
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
      RequestedQuotas = list(
        list(
          Id = "string",
          CaseId = "string",
          ServiceCode = "string",
          ServiceName = "string",
          QuotaCode = "string",
          QuotaName = "string",
          DesiredValue = 123.0,
          Status = "PENDING"|"CASE_OPENED"|"APPROVED"|"DENIED"|"CASE_CLOSED",
          Created = as.POSIXct(
            "2015-01-01"
          ),
          LastUpdated = as.POSIXct(
            "2015-01-01"
          ),
          Requester = "string",
          QuotaArn = "string",
          GlobalQuota = TRUE|FALSE,
          Unit = "string"
        )
      )
    )

### Request syntax

    svc$list_requested_service_quota_change_history(
      ServiceCode = "string",
      Status = "PENDING"|"CASE_OPENED"|"APPROVED"|"DENIED"|"CASE_CLOSED",
      NextToken = "string",
      MaxResults = 123
    )