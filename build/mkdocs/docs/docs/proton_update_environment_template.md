<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>proton_update_environment_template</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Update an environment template

### Description

Update an environment template.

### Usage

    proton_update_environment_template(description, displayName, name)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="proton_update_environment_template_:_description">description</code></td>
<td><p>A description of the environment template update.</p></td>
</tr>
<tr class="even">
<td><code
id="proton_update_environment_template_:_displayName">displayName</code></td>
<td><p>The name of the environment template to update as displayed in
the developer interface.</p></td>
</tr>
<tr class="odd">
<td><code
id="proton_update_environment_template_:_name">name</code></td>
<td><p>[required] The name of the environment template to
update.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      environmentTemplate = list(
        arn = "string",
        createdAt = as.POSIXct(
          "2015-01-01"
        ),
        description = "string",
        displayName = "string",
        encryptionKey = "string",
        lastModifiedAt = as.POSIXct(
          "2015-01-01"
        ),
        name = "string",
        provisioning = "CUSTOMER_MANAGED",
        recommendedVersion = "string"
      )
    )

### Request syntax

    svc$update_environment_template(
      description = "string",
      displayName = "string",
      name = "string"
    )