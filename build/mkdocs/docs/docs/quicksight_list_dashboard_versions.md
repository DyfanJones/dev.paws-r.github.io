<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>quicksight_list_dashboard_versions</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists all the versions of the dashboards in the Amazon QuickSight subscription

### Description

Lists all the versions of the dashboards in the Amazon QuickSight
subscription.

### Usage

    quicksight_list_dashboard_versions(AwsAccountId, DashboardId, NextToken,
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
id="quicksight_list_dashboard_versions_:_AwsAccountId">AwsAccountId</code></td>
<td><p>[required] The ID of the Amazon Web Services account that
contains the dashboard that you're listing versions for.</p></td>
</tr>
<tr class="even">
<td><code
id="quicksight_list_dashboard_versions_:_DashboardId">DashboardId</code></td>
<td><p>[required] The ID for the dashboard.</p></td>
</tr>
<tr class="odd">
<td><code
id="quicksight_list_dashboard_versions_:_NextToken">NextToken</code></td>
<td><p>The token for the next set of results, or null if there are no
more results.</p></td>
</tr>
<tr class="even">
<td><code
id="quicksight_list_dashboard_versions_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of results to be returned per
request.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      DashboardVersionSummaryList = list(
        list(
          Arn = "string",
          CreatedTime = as.POSIXct(
            "2015-01-01"
          ),
          VersionNumber = 123,
          Status = "CREATION_IN_PROGRESS"|"CREATION_SUCCESSFUL"|"CREATION_FAILED"|"UPDATE_IN_PROGRESS"|"UPDATE_SUCCESSFUL"|"UPDATE_FAILED"|"DELETED",
          SourceEntityArn = "string",
          Description = "string"
        )
      ),
      NextToken = "string",
      Status = 123,
      RequestId = "string"
    )

### Request syntax

    svc$list_dashboard_versions(
      AwsAccountId = "string",
      DashboardId = "string",
      NextToken = "string",
      MaxResults = 123
    )