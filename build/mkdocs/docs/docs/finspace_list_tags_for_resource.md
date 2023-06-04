<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>finspace_list_tags_for_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## A list of all tags for a resource

### Description

A list of all tags for a resource.

### Usage

    finspace_list_tags_for_resource(resourceArn)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="finspace_list_tags_for_resource_:_resourceArn">resourceArn</code></td>
<td><p>[required] The Amazon Resource Name of the resource.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      tags = list(
        "string"
      )
    )

### Request syntax

    svc$list_tags_for_resource(
      resourceArn = "string"
    )