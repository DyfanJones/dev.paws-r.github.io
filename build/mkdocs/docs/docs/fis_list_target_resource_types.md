<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>fis_list_target_resource_types</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists the target resource types

### Description

Lists the target resource types.

### Usage

    fis_list_target_resource_types(maxResults, nextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="fis_list_target_resource_types_:_maxResults">maxResults</code></td>
<td><p>The maximum number of results to return with a single call. To
retrieve the remaining results, make another call with the returned
<code>nextToken</code> value.</p></td>
</tr>
<tr class="even">
<td><code
id="fis_list_target_resource_types_:_nextToken">nextToken</code></td>
<td><p>The token for the next page of results.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      targetResourceTypes = list(
        list(
          resourceType = "string",
          description = "string"
        )
      ),
      nextToken = "string"
    )

### Request syntax

    svc$list_target_resource_types(
      maxResults = 123,
      nextToken = "string"
    )