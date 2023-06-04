<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>resiliencehub_update_app_version_app_component</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates an existing Application Component in the Resilience Hub application

### Description

Updates an existing Application Component in the Resilience Hub
application.

This API updates the Resilience Hub application draft version. To use
this Application Component for running assessments, you must publish the
Resilience Hub application using the `publish_app_version` API.

### Usage

    resiliencehub_update_app_version_app_component(additionalInfo, appArn,
      id, name, type)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="resiliencehub_update_app_version_app_component_:_additionalInfo">additionalInfo</code></td>
<td><p>Currently, there is no supported additional information for
Application Components.</p></td>
</tr>
<tr class="even">
<td><code
id="resiliencehub_update_app_version_app_component_:_appArn">appArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the Resilience Hub
application. The format for this ARN is:
arn:<code>partition</code>:resiliencehub:<code>region</code>:<code>account</code>:app/<code>app-id</code>.
For more information about ARNs, see <a
href="https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html">Amazon
Resource Names (ARNs)</a> in the <em>AWS General Reference</em>
guide.</p></td>
</tr>
<tr class="odd">
<td><code
id="resiliencehub_update_app_version_app_component_:_id">id</code></td>
<td><p>[required] The identifier of the Application Component.</p></td>
</tr>
<tr class="even">
<td><code
id="resiliencehub_update_app_version_app_component_:_name">name</code></td>
<td><p>The name of the Application Component.</p></td>
</tr>
<tr class="odd">
<td><code
id="resiliencehub_update_app_version_app_component_:_type">type</code></td>
<td><p>The type of Application Component. For more information about the
types of Application Component, see <a
href="https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html">Grouping
resources in an AppComponent</a>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      appArn = "string",
      appComponent = list(
        additionalInfo = list(
          list(
            "string"
          )
        ),
        id = "string",
        name = "string",
        type = "string"
      ),
      appVersion = "string"
    )

### Request syntax

    svc$update_app_version_app_component(
      additionalInfo = list(
        list(
          "string"
        )
      ),
      appArn = "string",
      id = "string",
      name = "string",
      type = "string"
    )