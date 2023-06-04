<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>connectwisdomservice_list_assistants</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists information about assistants

### Description

Lists information about assistants.

### Usage

    connectwisdomservice_list_assistants(maxResults, nextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="connectwisdomservice_list_assistants_:_maxResults">maxResults</code></td>
<td><p>The maximum number of results to return per page.</p></td>
</tr>
<tr class="even">
<td><code
id="connectwisdomservice_list_assistants_:_nextToken">nextToken</code></td>
<td><p>The token for the next set of results. Use the value returned in
the previous response in the next request to retrieve the next set of
results.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      assistantSummaries = list(
        list(
          assistantArn = "string",
          assistantId = "string",
          description = "string",
          name = "string",
          serverSideEncryptionConfiguration = list(
            kmsKeyId = "string"
          ),
          status = "CREATE_IN_PROGRESS"|"CREATE_FAILED"|"ACTIVE"|"DELETE_IN_PROGRESS"|"DELETE_FAILED"|"DELETED",
          tags = list(
            "string"
          ),
          type = "AGENT"
        )
      ),
      nextToken = "string"
    )

### Request syntax

    svc$list_assistants(
      maxResults = 123,
      nextToken = "string"
    )