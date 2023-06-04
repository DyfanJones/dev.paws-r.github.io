<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>backupgateway_untag_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Removes tags from the resource

### Description

Removes tags from the resource.

### Usage

    backupgateway_untag_resource(ResourceARN, TagKeys)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="backupgateway_untag_resource_:_ResourceARN">ResourceARN</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the resource from
which to remove tags.</p></td>
</tr>
<tr class="even">
<td><code
id="backupgateway_untag_resource_:_TagKeys">TagKeys</code></td>
<td><p>[required] The list of tag keys specifying which tags to
remove.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ResourceARN = "string"
    )

### Request syntax

    svc$untag_resource(
      ResourceARN = "string",
      TagKeys = list(
        "string"
      )
    )