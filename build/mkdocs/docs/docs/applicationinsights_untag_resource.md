<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>applicationinsights_untag_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Remove one or more tags (keys and values) from a specified application

### Description

Remove one or more tags (keys and values) from a specified application.

### Usage

    applicationinsights_untag_resource(ResourceARN, TagKeys)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="applicationinsights_untag_resource_:_ResourceARN">ResourceARN</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the application that
you want to remove one or more tags from.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationinsights_untag_resource_:_TagKeys">TagKeys</code></td>
<td><p>[required] The tags (tag keys) that you want to remove from the
resource. When you specify a tag key, the action removes both that key
and its associated tag value.</p>
<p>To remove more than one tag from the application, append the
<code>TagKeys</code> parameter and argument for each additional tag to
remove, separated by an ampersand.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$untag_resource(
      ResourceARN = "string",
      TagKeys = list(
        "string"
      )
    )