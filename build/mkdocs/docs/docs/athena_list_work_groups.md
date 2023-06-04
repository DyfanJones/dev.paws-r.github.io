<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>athena_list_work_groups</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists available workgroups for the account

### Description

Lists available workgroups for the account.

### Usage

    athena_list_work_groups(NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="athena_list_work_groups_:_NextToken">NextToken</code></td>
<td><p>A token generated by the Athena service that specifies where to
continue pagination if a previous request was truncated. To obtain the
next set of pages, pass in the <code>NextToken</code> from the response
object of the previous page call.</p></td>
</tr>
<tr class="even">
<td><code
id="athena_list_work_groups_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of workgroups to return in this
request.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      WorkGroups = list(
        list(
          Name = "string",
          State = "ENABLED"|"DISABLED",
          Description = "string",
          CreationTime = as.POSIXct(
            "2015-01-01"
          ),
          EngineVersion = list(
            SelectedEngineVersion = "string",
            EffectiveEngineVersion = "string"
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_work_groups(
      NextToken = "string",
      MaxResults = 123
    )