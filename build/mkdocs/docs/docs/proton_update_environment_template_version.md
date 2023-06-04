<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>proton_update_environment_template_version</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Update a major or minor version of an environment template

### Description

Update a major or minor version of an environment template.

### Usage

    proton_update_environment_template_version(description, majorVersion,
      minorVersion, status, templateName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="proton_update_environment_template_version_:_description">description</code></td>
<td><p>A description of environment template version to update.</p></td>
</tr>
<tr class="even">
<td><code
id="proton_update_environment_template_version_:_majorVersion">majorVersion</code></td>
<td><p>[required] To update a major version of an environment template,
include <code style="white-space: pre;">⁠major Version⁠</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="proton_update_environment_template_version_:_minorVersion">minorVersion</code></td>
<td><p>[required] To update a minor version of an environment template,
include <code>minorVersion</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="proton_update_environment_template_version_:_status">status</code></td>
<td><p>The status of the environment template minor version to
update.</p></td>
</tr>
<tr class="odd">
<td><code
id="proton_update_environment_template_version_:_templateName">templateName</code></td>
<td><p>[required] The name of the environment template.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      environmentTemplateVersion = list(
        arn = "string",
        createdAt = as.POSIXct(
          "2015-01-01"
        ),
        description = "string",
        lastModifiedAt = as.POSIXct(
          "2015-01-01"
        ),
        majorVersion = "string",
        minorVersion = "string",
        recommendedMinorVersion = "string",
        schema = "string",
        status = "REGISTRATION_IN_PROGRESS"|"REGISTRATION_FAILED"|"DRAFT"|"PUBLISHED",
        statusMessage = "string",
        templateName = "string"
      )
    )

### Request syntax

    svc$update_environment_template_version(
      description = "string",
      majorVersion = "string",
      minorVersion = "string",
      status = "REGISTRATION_IN_PROGRESS"|"REGISTRATION_FAILED"|"DRAFT"|"PUBLISHED",
      templateName = "string"
    )