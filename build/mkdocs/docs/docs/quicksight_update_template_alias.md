<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>quicksight_update_template_alias</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates the template alias of a template

### Description

Updates the template alias of a template.

### Usage

    quicksight_update_template_alias(AwsAccountId, TemplateId, AliasName,
      TemplateVersionNumber)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="quicksight_update_template_alias_:_AwsAccountId">AwsAccountId</code></td>
<td><p>[required] The ID of the Amazon Web Services account that
contains the template alias that you're updating.</p></td>
</tr>
<tr class="even">
<td><code
id="quicksight_update_template_alias_:_TemplateId">TemplateId</code></td>
<td><p>[required] The ID for the template.</p></td>
</tr>
<tr class="odd">
<td><code
id="quicksight_update_template_alias_:_AliasName">AliasName</code></td>
<td><p>[required] The alias of the template that you want to update. If
you name a specific alias, you update the version that the alias points
to. You can specify the latest version of the template by providing the
keyword <code style="white-space: pre;">⁠$LATEST⁠</code> in the
<code>AliasName</code> parameter. The keyword <code
style="white-space: pre;">⁠$PUBLISHED⁠</code> doesn't apply to
templates.</p></td>
</tr>
<tr class="even">
<td><code
id="quicksight_update_template_alias_:_TemplateVersionNumber">TemplateVersionNumber</code></td>
<td><p>[required] The version number of the template.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      TemplateAlias = list(
        AliasName = "string",
        Arn = "string",
        TemplateVersionNumber = 123
      ),
      Status = 123,
      RequestId = "string"
    )

### Request syntax

    svc$update_template_alias(
      AwsAccountId = "string",
      TemplateId = "string",
      AliasName = "string",
      TemplateVersionNumber = 123
    )