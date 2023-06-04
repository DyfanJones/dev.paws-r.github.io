<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>billingconductor_untag_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes specified tags from a resource

### Description

Deletes specified tags from a resource.

### Usage

    billingconductor_untag_resource(ResourceArn, TagKeys)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="billingconductor_untag_resource_:_ResourceArn">ResourceArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the resource to
which to delete tags.</p></td>
</tr>
<tr class="even">
<td><code
id="billingconductor_untag_resource_:_TagKeys">TagKeys</code></td>
<td><p>[required] The tags to delete from the resource as a list of
key-value pairs.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$untag_resource(
      ResourceArn = "string",
      TagKeys = list(
        "string"
      )
    )