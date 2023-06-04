<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>billingconductor_tag_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Associates the specified tags to a resource with the specified resourceArn

### Description

Associates the specified tags to a resource with the specified
`resourceArn`. If existing tags on a resource are not specified in the
request parameters, they are not changed.

### Usage

    billingconductor_tag_resource(ResourceArn, Tags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="billingconductor_tag_resource_:_ResourceArn">ResourceArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the resource to
which to add tags.</p></td>
</tr>
<tr class="even">
<td><code id="billingconductor_tag_resource_:_Tags">Tags</code></td>
<td><p>[required] The tags to add to the resource as a list of key-value
pairs.</p></td>
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