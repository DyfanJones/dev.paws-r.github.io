<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>panorama_tag_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Tags a resource

### Description

Tags a resource.

### Usage

    panorama_tag_resource(ResourceArn, Tags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="panorama_tag_resource_:_ResourceArn">ResourceArn</code></td>
<td><p>[required] The resource's ARN.</p></td>
</tr>
<tr class="even">
<td><code id="panorama_tag_resource_:_Tags">Tags</code></td>
<td><p>[required] Tags for the resource.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$tag_resource(
      ResourceArn = "string",
      Tags = list(
        "string"
      )
    )