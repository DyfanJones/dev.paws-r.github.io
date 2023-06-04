<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>workspacesweb_list_user_settings</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves a list of user settings

### Description

Retrieves a list of user settings.

### Usage

    workspacesweb_list_user_settings(maxResults, nextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="workspacesweb_list_user_settings_:_maxResults">maxResults</code></td>
<td><p>The maximum number of results to be included in the next
page.</p></td>
</tr>
<tr class="even">
<td><code
id="workspacesweb_list_user_settings_:_nextToken">nextToken</code></td>
<td><p>The pagination token used to retrieve the next page of results
for this operation.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      nextToken = "string",
      userSettings = list(
        list(
          copyAllowed = "Disabled"|"Enabled",
          disconnectTimeoutInMinutes = 123,
          downloadAllowed = "Disabled"|"Enabled",
          idleDisconnectTimeoutInMinutes = 123,
          pasteAllowed = "Disabled"|"Enabled",
          printAllowed = "Disabled"|"Enabled",
          uploadAllowed = "Disabled"|"Enabled",
          userSettingsArn = "string"
        )
      )
    )

### Request syntax

    svc$list_user_settings(
      maxResults = 123,
      nextToken = "string"
    )