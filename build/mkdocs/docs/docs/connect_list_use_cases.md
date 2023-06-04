<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>connect_list_use_cases</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists the use cases for the integration association

### Description

Lists the use cases for the integration association.

### Usage

    connect_list_use_cases(InstanceId, IntegrationAssociationId, NextToken,
      MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="connect_list_use_cases_:_InstanceId">InstanceId</code></td>
<td><p>[required] The identifier of the Amazon Connect instance. You can
<a
href="https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html">find
the instance ID</a> in the Amazon Resource Name (ARN) of the
instance.</p></td>
</tr>
<tr class="even">
<td><code
id="connect_list_use_cases_:_IntegrationAssociationId">IntegrationAssociationId</code></td>
<td><p>[required] The identifier for the integration
association.</p></td>
</tr>
<tr class="odd">
<td><code id="connect_list_use_cases_:_NextToken">NextToken</code></td>
<td><p>The token for the next set of results. Use the value returned in
the previous response in the next request to retrieve the next set of
results.</p></td>
</tr>
<tr class="even">
<td><code
id="connect_list_use_cases_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of results to return per page.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      UseCaseSummaryList = list(
        list(
          UseCaseId = "string",
          UseCaseArn = "string",
          UseCaseType = "RULES_EVALUATION"|"CONNECT_CAMPAIGNS"
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_use_cases(
      InstanceId = "string",
      IntegrationAssociationId = "string",
      NextToken = "string",
      MaxResults = 123
    )