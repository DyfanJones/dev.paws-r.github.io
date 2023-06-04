<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>macie2_get_sensitivity_inspection_template</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves the settings for the sensitivity inspection template for an account

### Description

Retrieves the settings for the sensitivity inspection template for an
account.

### Usage

    macie2_get_sensitivity_inspection_template(id)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="macie2_get_sensitivity_inspection_template_:_id">id</code></td>
<td><p>[required] The unique identifier for the Amazon Macie resource
that the request applies to.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      description = "string",
      excludes = list(
        managedDataIdentifierIds = list(
          "string"
        )
      ),
      includes = list(
        allowListIds = list(
          "string"
        ),
        customDataIdentifierIds = list(
          "string"
        ),
        managedDataIdentifierIds = list(
          "string"
        )
      ),
      name = "string",
      sensitivityInspectionTemplateId = "string"
    )

### Request syntax

    svc$get_sensitivity_inspection_template(
      id = "string"
    )