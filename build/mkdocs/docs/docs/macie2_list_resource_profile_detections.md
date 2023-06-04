<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>macie2_list_resource_profile_detections</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves information about the types and amount of sensitive data that Amazon Macie found in an S3 bucket

### Description

Retrieves information about the types and amount of sensitive data that
Amazon Macie found in an S3 bucket.

### Usage

    macie2_list_resource_profile_detections(maxResults, nextToken,
      resourceArn)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="macie2_list_resource_profile_detections_:_maxResults">maxResults</code></td>
<td><p>The maximum number of items to include in each page of a
paginated response.</p></td>
</tr>
<tr class="even">
<td><code
id="macie2_list_resource_profile_detections_:_nextToken">nextToken</code></td>
<td><p>The nextToken string that specifies which page of results to
return in a paginated response.</p></td>
</tr>
<tr class="odd">
<td><code
id="macie2_list_resource_profile_detections_:_resourceArn">resourceArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the S3 bucket that
the request applies to.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      detections = list(
        list(
          arn = "string",
          count = 123,
          id = "string",
          name = "string",
          suppressed = TRUE|FALSE,
          type = "CUSTOM"|"MANAGED"
        )
      ),
      nextToken = "string"
    )

### Request syntax

    svc$list_resource_profile_detections(
      maxResults = 123,
      nextToken = "string",
      resourceArn = "string"
    )